module Yi.Mode.Interactive where

import Control.Concurrent (threadDelay)
import Data.List (elemIndex)
import Prelude ()
import Yi.Modes
import Yi.Core
import Yi.History
import Yi.Lexer.Alex (Tok)
import Yi.Lexer.Compilation (Token)
import Yi.Region
import qualified Yi.Mode.Compilation as Compilation
import qualified Yi.Syntax.OnlineTree as OnlineTree

atLastLine :: BufferM Bool
atLastLine = savingPointB $ do
    moveToEol
    (==) <$> sizeB <*> pointB

mode :: Mode (OnlineTree.TreeAtPos (Tok Token))
mode = Compilation.mode
  { modeApplies = modeNeverApplies,
    modeName = "interactive",
    modeKeymap = (<||)
     (choice
      [spec KHome ?>>! ghciHome,
       spec KEnter ?>>! do
          eof <- withBuffer $ atLastLine
          if eof
            then feedCommand
            else withSyntax modeFollow,
       meta (char 'p') ?>>! interactHistoryMove 1,
       meta (char 'n') ?>>! interactHistoryMove (-1)
      ]) }

-- | The GHCi prompt always begins with ">"; this goes to just before it, or if one is already at the start
-- of the prompt, goes to the beginning of the line. (If at the beginning of the line, this pushes you forward to it.)
ghciHome :: BufferM ()
ghciHome = do l <- readLnB
              let epos = elemIndex '>' l
              case epos of
                  Nothing -> moveToSol
                  Just pos -> do (_,mypos) <- getLineAndCol
                                 if mypos == (pos+2) then moveToSol
                                  else moveToSol >> moveXorEol (pos+2)

interactId :: String
interactId = "Interact"

interactHistoryMove :: Int -> EditorM ()
interactHistoryMove delta = historyMoveGen interactId delta (withBuffer0 getInput) >>= (withBuffer0 . setInput)

interactHistoryFinish :: EditorM ()
interactHistoryFinish = historyFinishGen interactId (withBuffer0 getInput)

interactHistoryStart :: EditorM ()
interactHistoryStart = historyStartGen interactId

getInputRegion :: BufferM Region
getInputRegion = do mo <- getMarkB (Just "StdOUT")
                    p <- pointAt botB
                    q <- getMarkPointB mo
                    return $ mkRegion p q

getInput :: BufferM String
getInput = readRegionB =<< getInputRegion

setInput :: String -> BufferM ()
setInput val = flip replaceRegionB val =<< getInputRegion

-- | Open a new buffer for interaction with a process.
interactive :: String -> [String] -> YiM BufferRef
interactive cmd args = do
    b <- startSubprocess cmd args (const $ return ())
    withEditor $ interactHistoryStart
    mode' <- lookupMode $ AnyMode mode
    withBuffer $ do m1 <- getMarkB (Just "StdERR")
                    m2 <- getMarkB (Just "StdOUT")
                    modifyMarkB m1 (\v -> v {markGravity = Backward})
                    modifyMarkB m2 (\v -> v {markGravity = Backward})
                    setAnyMode mode'
    return b

-- | Send the type command to the process
feedCommand :: YiM ()
feedCommand = do
    b <- gets currentBuffer
    withEditor interactHistoryFinish
    cmd <- withBuffer $ do
        botB
        insertN "\n"
        me <- getMarkB (Just "StdERR")
        mo <- getMarkB (Just "StdOUT")
        p <- pointB
        q <- getMarkPointB mo
        cmd <- readRegionB $ mkRegion p q
        setMarkPointB me p
        setMarkPointB mo p
        return $ cmd
    withEditor interactHistoryStart
    sendToProcess b cmd



-- | Send command, recieve reply
queryReply :: BufferRef -> String -> YiM String
queryReply buf cmd = do
    start <- withGivenBuffer buf (botB >> pointB)
    sendToProcess buf (cmd ++ "\n")
    io $ threadDelay 50000  -- Hack to let ghci finish writing its output.
    withGivenBuffer buf $ do
        botB
        moveToSol
        leftB -- There is probably a much better way to do this moving around, but it works
        end <- pointB
        result <- readRegionB (mkRegion start end)
        botB
        return result

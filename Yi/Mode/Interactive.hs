module Yi.Mode.Interactive where

import Prelude ()
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.Core
import Yi.Core
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Prelude
import Yi.Region
import Yi.Lexer.Alex (Tok(..))
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Mode.Compilation         as Compilation
import qualified Yi.Syntax.Linear as Linear

atLastLine :: BufferM Bool
atLastLine = savingPointB $ do
    moveToEol
    (==) <$> sizeB <*> pointB

mode :: Mode (Linear.Result (Tok Compilation.Token))
mode = Compilation.mode
  { 
   modeKeymap = (<||) (spec KEnter ?>>! do
       eof <- withBuffer $ atLastLine
       if eof
         then feedCommand
         else withSyntax modeFollow)
  }

-- | Open a new buffer for interaction with a process.
interactive :: String -> [String] -> YiM BufferRef
interactive cmd args = do
    b <- startSubprocess cmd args (\_ -> return ())
    withBuffer $ do m1 <- getMarkB (Just "StdERR")
                    m2 <- getMarkB (Just "StdOUT")
                    modifyMarkB m1 (\v -> v {markGravity = Backward})
                    modifyMarkB m2 (\v -> v {markGravity = Backward})
                    setMode mode
    return b

-- | Send the type command to the process
feedCommand :: YiM ()
feedCommand = do
    b <- withEditor $ getBuffer
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
    sendToProcess b cmd


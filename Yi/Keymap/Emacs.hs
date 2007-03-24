--
-- Copyright (c) 2005,2007 Jean-Philippe Bernardy
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--



module Yi.Keymap.Emacs ( keymap, runKeymap, rebind, normalKeymap ) where

import Yi.Yi

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys
import Yi.Buffer
import Yi.Kernel
import Data.Char
import Data.Maybe
import Data.List
import Data.Dynamic
import qualified Yi.UI as UI  -- FIXME this module should not depend on UI

import Control.Monad
import Control.Monad.Trans

import Yi.Editor

-- * The keymap abstract definition

type KProc a = Interact Event a
type Process = KProc ()
type KList = [(String, Process)]

selfInsertKeymap :: Process
selfInsertKeymap = do
  Event (KASCII c) [] <- satisfy (const True)
  write (insertSelf c)

normalKeymap :: Process
normalKeymap = selfInsertKeymap +++ makeKeymap 
              [
        ("RET",      atomic $ repeatingArg $ insertE '\n'),
        ("DEL",      atomic $ repeatingArg deleteE),
        ("BACKSP",   atomic $ repeatingArg bdeleteE),
        ("C-M-w",    atomic $ appendNextKillE),
        ("C-/",      atomic $ repeatingArg undoE),
        ("C-_",      atomic $ repeatingArg undoE),
        ("C-<left>", atomic $ repeatingArg prevWordE),
        ("C-<right>",atomic $ repeatingArg nextWordE),
        ("C-@",    atomic $ (getPointE >>= setMarkE)), -- till vty correctly support C-SPC
        ("C-SPC",    atomic $ (getPointE >>= setMarkE)),
        ("C-a",      atomic $ repeatingArg solE),
        ("C-b",      atomic $ repeatingArg leftE),
        ("C-d",      atomic $ repeatingArg deleteE),
        ("C-e",      atomic $ repeatingArg eolE),
        ("C-f",      atomic $ repeatingArg rightE),
        ("C-g",      atomic $ unsetMarkE), 
--      ("C-g",      atomic $ keyboardQuitE), -- C-g should be a more general quit that also unsets the mark.
--      ("C-i",      atomic $ indentC),
        ("C-j",      atomic $ repeatingArg $ insertE '\n'),
        ("C-k",      atomic $ killLineE),
        ("C-m",      atomic $ repeatingArg $ insertE '\n'),
        ("C-n",      atomic $ repeatingArg downE),
        ("C-o",      atomic $ repeatingArg (insertE '\n' >> leftE)),
        ("C-p",      atomic $ repeatingArg upE),
        ("C-q",               insertNextC),
--      ("C-r",      atomic $ backwardsIncrementalSearchE),
--      ("C-s",      atomic $ incrementalSearchE),
        ("C-t",      atomic $ repeatingArg $ swapE),
        ("C-u",               readArgC),
        ("C-v",      atomic $ scrollDownE),
        ("C-w",      atomic $ killRegionE),
        ("C-z",      atomic $ suspendE),
        ("C-x ^",    atomic $ repeatingArg enlargeWinE),
        ("C-x 0",    atomic $ closeE),
        ("C-x 1",    atomic $ closeOtherE),
        ("C-x 2",    atomic $ splitE),
        ("C-x C-c",  atomic $ quitE),
        ("C-x C-f",  atomic $ findFile),
        ("C-x C-s",  atomic $ fwriteE),
        ("C-x C-x",  atomic $ exchangePointAndMarkE),
        ("C-x b",    atomic $ switchBufferE),
        ("C-x e e",  atomic $ evalRegionE),
        ("C-x o",    atomic $ nextWinE),
        ("C-x l",    atomic $ gotoLineE),
        ("C-x k",    atomic $ closeE),
--      ("C-x r k",  atomic $ killRectE),
--      ("C-x r o",  atomic $ openRectE),
--      ("C-x r t",  atomic $ stringRectE),
--      ("C-x r y",  atomic $ yankRectE),
        ("C-x u",    atomic $ repeatingArg undoE),
        ("C-x v",    atomic $ repeatingArg shrinkWinE),
        ("C-y",      atomic $ yankE),
        ("M-<",      atomic $ repeatingArg topE),
        ("M->",      atomic $ repeatingArg botE),
--      ("M-%",               searchReplaceC),
        ("M-BACKSP", atomic $ repeatingArg bkillWordE),
--      ("M-a",      atomic $ repeatingArg backwardSentenceE),
        ("M-b",      atomic $ repeatingArg prevWordE),
        ("M-c",      atomic $ repeatingArg capitaliseWordE),
        ("M-d",      atomic $ repeatingArg killWordE),
--      ("M-e",      atomic $ repeatingArg forwardSentenceE),
        ("M-f",      atomic $ repeatingArg nextWordE),
--      ("M-h",      atomic $ repeatingArg markParagraphE),
--      ("M-k",      atomic $ repeatingArg killSentenceE),
        ("M-l",      atomic $ repeatingArg lowercaseWordE),
--      ("M-t",      atomic $ repeatingArg transposeWordsE),
        ("M-u",      atomic $ repeatingArg uppercaseWordE),
        ("M-w",      atomic $ killRingSaveE),
        ("M-x",      atomic $ executeExtendedCommandE),
        ("M-y",      atomic $ yankPopE),
        ("<home>",   atomic $ repeatingArg solE),
        ("<end>",    atomic $ repeatingArg eolE),
        ("<left>",   atomic $ repeatingArg leftE),
        ("<right>",  atomic $ repeatingArg rightE),
        ("<up>",     atomic $ repeatingArg upE),
        ("<down>",   atomic $ repeatingArg downE),
        ("<next>",   atomic $ repeatingArg downScreenE),
        ("<prior>",  atomic $ repeatingArg upScreenE)
        ]

executeExtendedCommandE :: Action
executeExtendedCommandE = do
  withMinibuffer "M-x" completeFunctionName execE

evalRegionE :: Action
evalRegionE = do
  getRegionE >>= readRegionE >>= evalE


-- | Define an atomic interactive command.
-- Purose is to define "transactional" boundaries for killring, undo, etc.
atomic :: Action -> KProc ()
atomic cmd = write $ do cmd
                        killringEndCmd

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents


insertSelf :: Char -> Action
insertSelf c = repeatingArg $ insertNE [c]

insertNextC :: KProc ()
insertNextC = do c <- satisfy (const True)
                 write $ repeatingArg $ insertE (eventToChar c)

-- | C-u stuff
readArgC :: KProc ()
readArgC = do readArg' Nothing
              write $ do UniversalArg u <- getDynamic
                         lift $ logPutStrLn (show u)
                         msgE ""

readArg' :: Maybe Int -> KProc ()
readArg' acc = do
    write $ msgE $ "Argument: " ++ show acc
    c <- satisfy (const True) -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc

rebind :: [(String,Process)] -> Process -> Process
rebind keys kl = (choice [events (readKey k) >> p | (k,p) <- keys]) <++ kl

findFile :: Action
findFile = withMinibuffer "find file:" return $ \filename -> do 
             msgE $ "loading " ++ filename
             fnewE filename

-- | Goto a line specified in the mini buffer.
gotoLineE :: Action
gotoLineE = withMinibuffer "goto line:" return  $ gotoLnE . read

debug :: String -> Process
debug = write . lift . logPutStrLn

data History = History {historyCurrent :: Int, 
                        historyContents :: [String]} 
    deriving (Show, Typeable)
instance Initializable History where
    initial = return (History (-1) [])
    

historyUp :: EditorM ()
historyUp = historyMove 1

historyDown :: EditorM () 
historyDown = historyMove (-1)

historyStart :: EditorM ()
historyStart = do
  (History _cur cont) <- getDynamic
  curValue <- readAllE
  setDynamic (History 0 (nub ("":cont)))
  debugHist

historyFinish :: EditorM ()
historyFinish = do
  (History _cur cont) <- getDynamic
  curValue <- readAllE
  setDynamic $ History (-1) (nub $ dropWhile null $ (curValue:cont))

debugHist = do
  h :: History <- getDynamic
  lift $ logPutStrLn (show h)

historyMove :: Int -> EditorM ()
historyMove delta = do
  (History cur cont) <- getDynamic
  curValue <- readAllE
  let len = length cont
      next = cur + delta
      nextValue = cont !! next
  case (next < 0, next >= len) of
    (True, _) -> msgE "end of history, no next item."
    (_, True) -> msgE "beginning of history, no previous item."
    (_,_) -> do 
         setDynamic (History next (take cur cont ++ [curValue] ++ drop (cur+1) cont))
         debugHist
         withBuffer $ \b -> do
              sz <- sizeB b
              moveTo b 0
              deleteN b sz
              insertN b nextValue

commonPrefix :: [String] -> String
commonPrefix [] = []
commonPrefix strings 
    | any null strings = []
    | all (== prefix) heads = prefix : commonPrefix tails
    | otherwise = []
    where 
          (heads, tails) = unzip [(h,t) | (h:t) <- strings]
          prefix = head heads
-- for an alternative implementation see GHC's InteractiveUI module.

completeInList :: [String] -> String -> String
completeInList l s = if null prefix then s else prefix
    where prefix = commonPrefix (filter (s `isPrefixOf`) l)

completeBufferName :: String -> EditorM String
completeBufferName s = do
  bs <- getBuffers
  return $ completeInList (map nameB bs) s

completeFunctionName :: String -> EditorM String
completeFunctionName s = do
  names <- getNamesInScopeE
  return $ completeInList names s

completionFunction :: (String -> EditorM String) -> EditorM ()
completionFunction f = do
  p <- getPointE
  gotoPointE 0
  text <- readNM 0 p
  deleteNE p
  compl <- f text
  insertNE compl

withMinibuffer :: String -> (String -> EditorM String) -> (String -> Action) -> Action
withMinibuffer prompt completer act = do 
  historyStart
  spawnMinibufferE prompt (runKeymap (rebind rebindings normalKeymap))
    -- | Read contents of current buffer (which should be the minibuffer), and
    -- apply it to the desired action
    where innerAction :: Action
          innerAction = do historyFinish
                           lineString <- readAllE
                           closeE
                           act lineString
          rebindings = [("RET", write innerAction),
                        ("M-p", write historyUp),
                        ("M-n", write historyDown),
                        ("<up>", write historyUp),
                        ("<down>", write historyDown),
                        ("C-i", write (completionFunction completer)),
                        ("C-g", write closeE)]

scrollDownE :: Action
scrollDownE = withUnivArg $ \a ->
              case a of
                 Nothing -> downScreenE
                 Just n -> replicateM_ n downE

switchBufferE :: Action
switchBufferE = withMinibuffer "switch to buffer:" completeBufferName $ \bufName -> do
                  b <- getBuffer -- current buffer
                  bs <- readEditor $ \e -> findBufferWithName e bufName
                  case filter (/= b) bs of
                    [] -> errorE "No such buffer"
                    (b':_) -> getWindow >>= UI.setWindowBuffer b'

-- | Create a binding processor from 'kmap'.
makeKeymap :: KList -> KProc ()
makeKeymap kmap = choice [events (readKey k) >> a | (k,a) <- kmap]

-- | entry point
keymap :: Keymap
keymap = runKeymap normalKeymap


showFailures :: Process -> Process
showFailures p = do result <- consumeLookahead p 
                    case result of
                      Right _ -> return ()
                      Left e -> write $ errorE $ "Key not bound: " ++ showKey e

runKeymap :: Process -> Keymap
runKeymap km evs = (setSynE "haskell" : runProcess (forever $ showFailures km) evs)

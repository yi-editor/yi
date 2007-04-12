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

-- This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when
-- convenient. Hence, do not go into the trouble of trying 100%
-- emulation. For example, M-x gives access to Yi (haskell) functions,
-- with their native names.

module Yi.Keymap.Emacs ( keymap, makeProcess, runKeymap, rebind, normalKeymap, withMinibuffer ) where

import Yi.Yi

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys
import Yi.Buffer
import Data.Char
import Data.Maybe
import Data.List
import qualified Yi.UI as UI  -- FIXME this module should not depend on UI

import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory

import Yi.Editor
import Yi.History

-- * The keymap abstract definition

type KProc a = Interact Event a
type Process = KProc ()
type KList = [(String, Process)]

selfInsertKeymap :: Process
selfInsertKeymap = do
  Event (KASCII c) [] <- satisfy isPrintableEvent
  write (insertSelf c)
      where isPrintableEvent (Event (KASCII c) []) = c >= ' '
            isPrintableEvent _ = False

normalKeymap :: Process
normalKeymap = selfInsertKeymap +++ makeProcess 
              [
        ("TAB",      atomic $ autoIndentE),
        ("RET",      atomic $ repeatingArg $ insertE '\n'),
        ("DEL",      atomic $ repeatingArg deleteE),
        ("BACKSP",   atomic $ repeatingArg bdeleteE),
        ("C-M-w",    atomic $ appendNextKillE),
        ("C-/",      atomic $ repeatingArg undoE),
        ("C-_",      atomic $ repeatingArg undoE),
        ("C-<left>", atomic $ repeatingArg prevWordE),
        ("C-<right>",atomic $ repeatingArg nextWordE),
        ("C-@",      atomic $ (getPointE >>= setMarkE)),
        ("C-SPC",    atomic $ (getPointE >>= setMarkE)),
        ("C-a",      atomic $ repeatingArg solE),
        ("C-b",      atomic $ repeatingArg leftE),
        ("C-d",      atomic $ repeatingArg deleteE),
        ("C-e",      atomic $ repeatingArg eolE),
        ("C-f",      atomic $ repeatingArg rightE),
        ("C-g",      atomic $ unsetMarkE), 
--      ("C-g",      atomic $ keyboardQuitE), -- C-g should be a more general quit that also unsets the mark.
        ("C-i",      atomic $ autoIndentE),
        ("C-j",      atomic $ repeatingArg $ insertE '\n'),
        ("C-k",      atomic $ killLineE),
        ("C-m",      atomic $ repeatingArg $ insertE '\n'),
        ("C-n",      atomic $ repeatingArg downE),
        ("C-o",      atomic $ repeatingArg (insertE '\n' >> leftE)),
        ("C-p",      atomic $ repeatingArg upE),
        ("C-q",               insertNextC),
--      ("C-r",      atomic $ backwardsIncrementalSearchE),
        ("C-s",      isearchProcess),
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
        ("C-x C-w",  atomic $ withMinibuffer "Write file: " completeFileName fwriteToE),
        ("C-x C-x",  atomic $ exchangePointAndMarkE),
        ("C-x b",    atomic $ switchBufferE),
        ("C-x e e",  atomic $ evalRegionE),
        ("C-x o",    atomic $ nextWinE),
        ("C-x l",    atomic $ gotoLineE),
        ("C-x k",    atomic $ killBufferE),
--      ("C-x r k",  atomic $ killRectE),
--      ("C-x r o",  atomic $ openRectE),
--      ("C-x r t",  atomic $ stringRectE),
--      ("C-x r y",  atomic $ yankRectE),
        ("C-x u",    atomic $ repeatingArg undoE),
        ("C-x v",    atomic $ repeatingArg shrinkWinE),
        ("C-y",      atomic $ yankE),
        ("M-<",      atomic $ repeatingArg topE),
        ("M->",      atomic $ repeatingArg botE),
        ("M-%",      atomic $ queryReplaceE),
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


----------------------------
-- autoindent

savingExcursion :: EditorM a -> EditorM a
savingExcursion f = do
    p <- getPointE
    res <- f
    gotoPointE p
    return res

getPreviousLineE :: EditorM String
getPreviousLineE = savingExcursion $ do
                     upE
                     readLnE

fetchPreviousIndentsE :: EditorM [Int]
fetchPreviousIndentsE = do
  p0 <- getPointE
  upE
  p1 <- getPointE
  l <- readLnE
  let i = indentOf l
  if p0 == p1 || indentOf l == 0 then return [0] else do
    is <- fetchPreviousIndentsE
    return (i:is)
    
cycleIndentsE :: [Int] -> EditorM ()
cycleIndentsE indents = do
  l <- readLnE
  let curIndent = indentOf l
  let (below, above) = span (< curIndent) $ indents
  msgE $ show (below, above)
  indentToE $ last (above ++ below)

autoIndentE :: EditorM ()
autoIndentE = do
  is <- savingExcursion fetchPreviousIndentsE
  pl <- getPreviousLineE
  let pli = indentOf pl
  cycleIndentsE $ sort $ nub $ pli+2 : is

indentOf :: String -> Int
indentOf = spacingOf . takeWhile isSpace

spacingOf :: String -> Int
spacingOf = sum . map spacingOfChar
    where spacingOfChar '\t' = 8
          spacingOfChar _ = 1

indentToE :: Int -> EditorM ()
indentToE level = do 
  l <- readLnE
  solE
  killE
  insertNE (replicate level ' ' ++ dropWhile isSpace l)


-----------------------------
-- isearch

selfSearchKeymap :: Process
selfSearchKeymap = do
  Event (KASCII c) [] <- satisfy (const True)
  write (isearchAddE [c])

searchKeymap :: Process
searchKeymap = 
    selfSearchKeymap +++ makeProcess 
       [("C-s", write isearchNextE),
        ("C-g", write isearchDelE),
        ("BACKSP", write isearchDelE)]
                 
isearchProcess :: Process
isearchProcess = do 
  write isearchInitE
  many' searchKeymap
  optional' $ choice $ map (events . readKey) $ ["C-m", "RET"]
  write isearchFinishE


----------------------------
-- query-replace

-- This implementation of query-replace suffers from (at least) two
-- problems: 1. the user doesn't clearly see what is going to be
-- replaced. 2. the user must quit the current minibuffer with 'q'
-- explicitly before resuming edit.

queryReplaceE :: EditorM ()
queryReplaceE = do
    withMinibuffer "Replace:" return $ \replaceWhat -> do
    withMinibuffer "With:" return $ \replaceWith -> do
    b <- withBuffer return
    let replaceBindings = [("n", write $ qrNextE b replaceWhat),
                           ("y", write $ qrReplaceOneE b replaceWhat replaceWith),
                           ("q", write $ closeE),
                           ("C-g", write $ closeE)
                           ]
    mp <- lift $ searchB b replaceWhat
    case mp of
      Nothing -> msgE "String to search not found"
      Just p -> do
        gotoPointE p
        spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q):")
            (runKeymap $ makeProcess replaceBindings)


----------------------------

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
rebind keys kl = makeProcess keys <++ kl

findFile :: Action
findFile = withMinibuffer "find file:" completeFileName $ \filename -> do 
             msgE $ "loading " ++ filename
             fnewE filename

-- | Goto a line specified in the mini buffer.
gotoLineE :: Action
gotoLineE = withMinibuffer "goto line:" return  $ gotoLnE . read

debug :: String -> Process
debug = write . lift . logPutStrLn

commonPrefix :: [String] -> String
commonPrefix [] = []
commonPrefix strings 
    | any null strings = []
    | all (== prefix) heads = prefix : commonPrefix tailz
    | otherwise = []
    where 
          (heads, tailz) = unzip [(h,t) | (h:t) <- strings]
          prefix = head heads
-- for an alternative implementation see GHC's InteractiveUI module.

completeInList :: String -> [String] -> EditorM String
completeInList s l 
    | null filtered = msgE "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = msgE "Sole completion" >> return s
    | prefix `elem` filtered = msgE ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = msgE ("Matches: " ++ show filtered) >> return s
    where prefix = commonPrefix filtered
          filtered = filter (s `isPrefixOf`) l
          isSingleton [_] = True
          isSingleton _ = False

completeBufferName :: String -> EditorM String
completeBufferName s = do
  bs <- getBuffers
  completeInList s (map nameB bs)

completeFileName :: String -> EditorM String
completeFileName s0 = do
  curDir <- lift $ getCurrentDirectory
  homeDir <- lift $ getHomeDirectory
  let s = if (['~',pathSeparator] `isPrefixOf` s0) then addTrailingPathSeparator homeDir ++ drop 2 s0 else s0
      sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir else sDir
      fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- lift $ getDirectoryContents searchDir
  fs <- lift $ mapM fixTrailingPathSeparator files
  completeInList s $ map (sDir </>) fs

completeFunctionName :: String -> EditorM String
completeFunctionName s = do
  names <- getNamesInScopeE
  completeInList s names

completionFunction :: (String -> EditorM String) -> EditorM ()
completionFunction f = do
  p <- getPointE
  text <- readNM 0 p
  compl <- f text 
  -- it's important to do this before removing the text, 
  -- so if the completion function raises an exception, we don't delete the buffer contents.
  gotoPointE 0
  deleteNE p
  insertNE compl

withMinibuffer :: String -> (String -> EditorM String) -> (String -> Action) -> Action
withMinibuffer prompt completer act = do 
  initialBuffer <- getBuffer
  Just initialWindow <- getWindow
  let innerAction :: Action
      innerAction = do historyFinish
                       lineString <- readAllE
                       closeMinibuffer
                       UI.setWindow initialWindow
                       switchToBufferE initialBuffer 
                       -- The above ensures that the action is performed on the buffer that originated the minibuffer.
                       act lineString
      rebindings = [("RET", write innerAction),
                    ("C-m", write innerAction),
                    ("M-p", write historyUp),
                    ("M-n", write historyDown),
                    ("<up>", write historyUp),
                    ("<down>", write historyDown),
                    ("C-i", write (completionFunction completer)),
                    ("TAB", write (completionFunction completer)),
                    ("C-g", write closeMinibuffer)]
  historyStart
  spawnMinibufferE prompt (runKeymap (rebind rebindings normalKeymap))
    -- | Read contents of current buffer (which should be the minibuffer), and
    -- apply it to the desired action
    where closeMinibuffer = do b <- getBuffer; closeE; deleteBuffer b 

scrollDownE :: Action
scrollDownE = withUnivArg $ \a ->
              case a of
                 Nothing -> downScreenE
                 Just n -> replicateM_ n downE

switchBufferE :: Action
switchBufferE = withMinibuffer "switch to buffer:" completeBufferName switchToBufferWithNameE

killBufferE :: Action
killBufferE = withMinibuffer "kill buffer:" completeBufferName $ \bufName -> do
                nextB <- nextBuffer
                b <- getBuffer
                b' <- if null bufName then return b else getBufferWithName bufName
                switchToBufferE nextB
                deleteBuffer b'

-- | Create a binding processor from 'kmap'.
makeProcess :: KList -> KProc ()
makeProcess kmap = choice [events (readKey k) >> a | (k,a) <- kmap]

-- | entry point
keymap :: Keymap
keymap = runKeymap normalKeymap


showFailures :: Process -> Process
showFailures p = do result <- consumeLookahead p 
                    case result of
                      Right _ -> return ()
                      Left e -> write $ errorE $ "Key not bound: " ++ showKey e

-- NOTE: showFailures is unused because its error-recovery mechanism
-- is in adequate when C-s (isearch) is implemented. When you type a
-- key not recognized by isearch, it automatically reverts to the
-- default keymap binding. This can be very simply implemented by
-- embedding the whole keymap in a "forever" construct. However, in
-- that case, when a altogether invalid key sequence is typed, the
-- keymap crashes without a message for the user.

-- The solution would be to make runProcess return the pending events
-- when crashing.

runKeymap :: Process -> Keymap
runKeymap km evs = (setSynE "haskell" : runProcess (forever $ km) evs)

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

{-
  This module is aimed at being a helper for the Emacs keybindings.
  In particular this should be useful for anyone that has a custom
  keymap derived from or based on the Emacs one.
-}

module Yi.Keymap.Emacs.Utils 
  ( Process        
  , makeProcess
  , completeWordB
  , completeInList

  , runKeymap
  , rebind
  , withMinibuffer
  , atomic
  , queryReplaceE
  , isearchProcess
  , shellCommandE
  , executeExtendedCommandE 
  , evalRegionE 
  , readArgC
  , gotoLineE
  , scrollDownE
  , switchBufferE
  , killBufferE
  , insertSelf
  , insertNextC
  , findFile
  , completeFileName
  ) 
where

{- Standard Library Module Imports -}
import Control.Monad
  ()
import Control.Monad.Trans
  ( lift
  , liftIO
  )
import Data.Char
  ( ord
  , isDigit
  )
import Data.List
  ( isPrefixOf )
import Data.Maybe
  ( fromMaybe )

import System.FilePath
  ( takeDirectory
  , isAbsolute
  , pathSeparator
  , (</>)
  , addTrailingPathSeparator
  , splitDirectories
  , joinPath
  , normalise
  , hasTrailingPathSeparator
  )
import System.Directory
  ( doesDirectoryExist
  , getHomeDirectory
  , getCurrentDirectory
  , getDirectoryContents
  )

{- External Library Module Imports -}
{- Local (yi) module imports -}
import Yi.Yi

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys
import Yi.Buffer

import Yi.Editor
import Yi.History


{- End of Module Imports -}

-- * The keymap abstract definition

type KProc a = Interact Event a
type Process = KProc ()
type KList = [(String, Process)]


-- | Create a binding processor from 'kmap'.
makeProcess :: KList -> KProc ()
makeProcess kmap = choice [events (readKey k) >> a | (k,a) <- kmap]

-- Commenting out to avoid compiler-warnings about unused function
-- showFailures :: Process -> Process
-- showFailures p = do result <- consumeLookahead p
--                     case result of
--                       Right _ -> return ()
--                       Left e -> write $ errorE $ "Key not bound: " ++ showKey e

-- NOTE: showFailures is unused because its error-recovery mechanism
-- is in adequate when C-s (isearch) is implemented. When you type a
-- key not recognized by isearch, it automatically reverts to the
-- default keymap binding. This can be very simply implemented by
-- embedding the whole keymap in a "forever" construct. However, in
-- that case, when a altogether invalid key sequence is typed, the
-- keymap crashes without a message for the user.

-- The solution would be to make runProcess return the pending events
-- when crashing.

-------------------------------------------
-- General completion
-------------------------------------------
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

completeInList :: String -> (String -> Bool) -> [ String ] -> YiM String
completeInList s condition l
    | null filtered = msgE "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = msgE "Sole completion" >> return s
    | prefix `elem` filtered = msgE ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = msgE ("Matches: " ++ show filtered) >> return s
    where prefix = commonPrefix filtered
          filtered = filter condition l
          isSingleton [_] = True
          isSingleton _ = False

----------------------------
-- Alternative Word Completion

{-
  'completeWordB' is an alternative to 'Yi.CharMove.wordCompleteB'.
  Currently the main reason for this extra function is that the
  aforementioned is rather buggy, two problems I currently have with
  it is that it occasionally remembers the previous word it was completing
  before and completes that rather than the current one. More seriously
  it occasionally crashes yi by going into an infinite loop.

  In the longer term assuming that 'Yi.CharMove.wordCompleteB' is fixed
  (which I would love) then 'completeWordB' offers a slightly different
  interface. The user completes the word using the mini-buffer in the
  same way a user completes a buffer or file name when switching buffers
  or opening a file. This means that it never guesses and completes
  only as much as it can without guessing.

  I think there is room for both approaches. The 'wordCompleteB' approach
  which just guesses the completion from a list of possible completion
  and then re-hitting the key-binding will cause it to guess again.
  I think this is very nice for things such as completing a word within
  a tex-buffer. However using the mini-buffer might be nicer when we allow
  syntax knowledge to allow completion for example we may complete from
  a hoogle database.
-}
completeWordB :: YiM ()
completeWordB = veryQuickCompleteWord


{-
  This is a very quick and dirty way to complete the current word.
  It works in a similar way to the completion of words in the mini-buffer
  it uses the message buffer to give simple feedback such as,
  "Matches:" and "Complete, but not unique:"

  It is by no means perfect but it's also not bad, pretty usable.
-}
veryQuickCompleteWord :: YiM ()
veryQuickCompleteWord =
  do (curWord, curWords) <- withBuffer wordsAndCurrentWord
     let condition :: String -> Bool
         condition x   = (isPrefixOf curWord x) && (x /= curWord)
     preText             <- completeInList curWord condition curWords
     if curWord == ""
        then msgE "No word to complete"
        else withBuffer $ insertN $ drop (length curWord) preText

wordsAndCurrentWord :: BufferM (String, [ String ])
wordsAndCurrentWord =
  do curSize          <- sizeB
     curText          <- readNM 0 curSize
     (curWord, _, _)  <- readWordLeftB
     let curWords     = words curText
     return (curWord, curWords)

{-
  Finally obviously we wish to have a much more sophisticated completeword.
  One which spawns a mini-buffer and allows searching in hoogle databases
  or in other files etc.
-}

---------------------------


----------------------------
-- shell-command

shellCommandE :: YiM ()
shellCommandE = do
    withMinibuffer "Shell command:" return $ \command -> do
    pipeE command "" >>= newBufferE "*Shell Command Output*" >> return ()


-----------------------------
-- isearch

selfSearchKeymap :: Process
selfSearchKeymap = do
  Event (KASCII c) [] <- satisfy (const True)
  write (isearchAddE [c])

searchKeymap :: Process
searchKeymap = 
    selfSearchKeymap +++ makeProcess 
        [--("C-g", isearchDelE), -- Only if string is not empty.
         ("C-s", write isearchNextE),
         ("C-w", write isearchWordE),
         ("BACKSP", write $ isearchDelE)]
                 
isearchProcess :: Process
isearchProcess = do 
  write isearchInitE
  many' searchKeymap
  foldr1 (<++) [events (readKey "C-g") >> write isearchCancelE,
                events (readKey "C-m") >> write isearchFinishE,
                events (readKey "RET") >> write isearchFinishE,
                write isearchFinishE]



----------------------------
-- query-replace

queryReplaceE :: YiM ()
queryReplaceE = do
    withMinibuffer "Replace:" return $ \replaceWhat -> do
    withMinibuffer "With:" return $ \replaceWith -> do
    b <- withEditor $ getBuffer
    let replaceBindings = [("n", write $ qrNextE b replaceWhat),
                           ("y", write $ qrReplaceOneE b replaceWhat replaceWith),
                           ("q", write $ closeE),
                           ("C-g", write $ closeE)
                           ]
    spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q):")
            (const (makeProcess replaceBindings))
            (qrNextE b replaceWhat)



executeExtendedCommandE :: YiM ()
executeExtendedCommandE = do
  withMinibuffer "M-x" completeFunctionName execE

evalRegionE :: YiM ()
evalRegionE = do
  withBuffer (getRegionB >>= readRegionB) >>= evalE


-- | Define an atomic interactive command.
-- Purose is to define "transactional" boundaries for killring, undo, etc.
atomic :: (Show x, YiAction a) => a x -> KProc ()
atomic cmd = write $ do runAction (makeAction cmd)
                        killringEndCmd

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents


insertSelf :: Char -> YiM ()
insertSelf = repeatingArg . insertB

insertNextC :: KProc ()
insertNextC = do c <- satisfy (const True)
                 write $ repeatingArg $ insertB (eventToChar c)

-- | C-u stuff
readArgC :: KProc ()
readArgC = do readArg' Nothing
              write $ do UniversalArg u <- getDynamic
                         logPutStrLn (show u)
                         msgE ""

readArg' :: Maybe Int -> KProc ()
readArg' acc = do
    write $ msgE $ "Argument: " ++ show acc
    c <- satisfy (const True) -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc

rebind :: [(String,Process)] -> KeymapMod
rebind keys = (makeProcess keys <++)

findFile :: YiM ()
findFile = do maybePath <- withBuffer getfileB
              startPath <- liftIO $ getFolder maybePath
              withMinibuffer "find file:" (completeFileName (Just startPath)) $ \filename -> do {
               let filename' = fixFilePath startPath filename
             ; msgE $ "loading " ++ filename'
             ; fnewE filename'
                                                                                                  }


-- | Fix entered file path by prepending the start folder if necessary,
-- | removing .. bits, and normalising.
fixFilePath :: String -> String -> String
fixFilePath start path = 
    let path' = if isAbsolute path then path else start </> path
    in (normalise . joinPath . dropDotDot . splitDirectories) path'

-- | turn a/b/../c into a/c etc
dropDotDot :: [String] -> [String]
dropDotDot pathElems = case ddd pathElems of
                         (False, result) -> result
                         (True, result)  -> dropDotDot result
    where ddd [] = (False, [])
          ddd ("/":"..":xs) = (True, "/":xs)
          ddd (_:"..":xs) = (True, xs)
          ddd (x:xs) = let (changed, rest) = ddd xs
                       in (changed, x:rest)

-- | Given a path, trim the file name bit if it exists.  If no path given,
-- | return current directory
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir


-- | Goto a line specified in the mini buffer.
gotoLineE :: YiM ()
gotoLineE = withMinibuffer "goto line:" return  $ (\s -> withBuffer (gotoLn (read s) >> return ()))

-- debug :: String -> Process
-- debug = write . logPutStrLn

completeBufferName :: String -> YiM String
completeBufferName s = do
  bs <- withEditor getBuffers
  completeInList s (isPrefixOf s) (map name bs)

completeFileName :: Maybe String -> String -> YiM String
completeFileName start s0 = do
  curDir <- case start of Nothing -> do bufferPath <- withBuffer getfileB
                                        liftIO $ getFolder bufferPath
                          (Just path) -> return path
  homeDir <- lift $ getHomeDirectory
  let s = if (['~',pathSeparator] `isPrefixOf` s0) then addTrailingPathSeparator homeDir ++ drop 2 s0 else s0
      sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir 
                  else if isAbsolute sDir then sDir
                  else curDir </> sDir
      fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- lift $ getDirectoryContents searchDir
  fs <- lift $ mapM fixTrailingPathSeparator files
  completeInList s (isPrefixOf s) $ map (sDir </>) fs

completeFunctionName :: String -> YiM String
completeFunctionName s = do
  names <- getNamesInScopeE
  completeInList s (isPrefixOf s) names

completionFunction :: (String -> YiM String) -> YiM ()
completionFunction f = do
  p <- withBuffer pointB
  text <- withBuffer $ readNM 0 p
  compl <- f text 
  -- it's important to do this before removing the text, 
  -- so if the completion function raises an exception, we don't delete the buffer contents.
  withBuffer $ do moveTo 0
                  deleteN p
                  insertN compl

withMinibuffer :: String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt completer act = do 
  initialBuffer <- withEditor getBuffer
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = do b <- withEditor getBuffer; closeE; withEditor (deleteBuffer b);
      innerAction = do historyFinish
                       lineString <- withBuffer elemsB
                       closeMinibuffer
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
  spawnMinibufferE (prompt ++ " ") (rebind rebindings) (return ())

scrollDownE :: YiM ()
scrollDownE = withUnivArg $ \a ->
              case a of
                 Nothing -> downScreenE
                 Just n -> withBuffer $ replicateM_ n lineDown

switchBufferE :: YiM ()
switchBufferE = withMinibuffer "switch to buffer:" completeBufferName switchToBufferWithNameE

killBufferE :: YiM ()
killBufferE = withMinibuffer "kill buffer:" completeBufferName closeBufferE


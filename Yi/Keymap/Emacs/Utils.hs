{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

{-
  This module is aimed at being a helper for the Emacs keybindings.
  In particular this should be useful for anyone that has a custom
  keymap derived from or based on the Emacs one.
-}

module Yi.Keymap.Emacs.Utils
  ( KList
  , makeKeymap
  , makePartialKeymap

  , askQuitEditor
  , modifiedQuitEditor
  , adjIndent
  , changeBufferNameE
  , rebind
  , withMinibuffer
  , queryReplaceE
  , isearchKeymap
  , shellCommandE
  , cabalConfigureE
  , cabalBuildE
  , reloadProjectE
  , executeExtendedCommandE
  , evalRegionE
  , readArgC
  , scrollDownE
  , scrollUpE
  , switchBufferE
  , killBufferE
  , insertSelf
  , insertNextC
  , insertTemplate
  , findFile
  , matchingFileNames
  , matchingBufferNames
  )
where

{- Standard Library Module Imports -}
import Control.Monad
  ()
import Data.Char
  ( ord
  , isDigit
  )
import Data.List
  ( isPrefixOf
  , (\\)
  )
import Data.Maybe
  ( fromMaybe )
import System.Exit
  ( ExitCode( ExitSuccess,ExitFailure ) )
import System.FriendlyPath
import System.FilePath
  ( takeDirectory
  , (</>)
  , addTrailingPathSeparator
  , hasTrailingPathSeparator
  , takeFileName
  )
import System.Directory
  ( doesDirectoryExist
  , getDirectoryContents
  , getCurrentDirectory
  , setCurrentDirectory
  )
import Control.Monad.Trans (MonadIO (..))
import Control.Monad
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Control.Applicative
import Control.Monad
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.Core
import Yi.Debug
import Yi.Dired
import Yi.Editor
import Yi.Eval
import Yi.Event
import Yi.File
import Yi.Interact hiding (write)
import Yi.Keymap.Emacs.Keys
import Yi.Keymap.Emacs.UnivArgument
import Yi.MiniBuffer
import Yi.Process
import Yi.Regex
import Yi.Search
import Yi.Templates
  ( addTemplate
  , templateNames
  )
import Yi.UI.Common 
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

{- End of Module Imports -}

----------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are unmodified buffers then we ask individually for
-- each modified buffer whether or not the user wishes to save
-- it or not. If we get to the end of this list and there are still
-- some modified buffers then we ask again if the user wishes to
-- quit, but this is then a simple yes or no.
askQuitEditor :: YiM ()
askQuitEditor = askIndividualQuit =<< getModifiedBuffers

getModifiedBuffers :: YiM [FBuffer]
getModifiedBuffers = filterM isFileBuffer =<< filter (not . isUnchangedBuffer) <$> withEditor getBuffers

-- | Is there a proper file associated with the buffer?
-- In other words, does it make sense to offer to save it?
isFileBuffer :: (Functor m, MonadIO m) => FBuffer -> m Bool
isFileBuffer b = case file b of
                   Nothing -> return False
                   Just fn -> not <$> liftIO (doesDirectoryExist fn)
                     
--------------------------------------------------
-- Takes in a list of buffers which have been identified
-- as modified since their last save.
askIndividualQuit :: [FBuffer] -> YiM ()
askIndividualQuit [] = modifiedQuitEditor
askIndividualQuit (firstBuffer : others) =
  withEditor (spawnMinibufferE saveMessage askKeymap) >> return ()
  where
  askKeymap   = const $ makeKeymap askBindings
  saveMessage = concat [ "do you want to save the buffer: "
                       , bufferName
                       , "? (y/n/q/c)"
                       ]
  bufferName  = name firstBuffer

  askBindings = [ ("n", write noAction)
                , ( "y", write yesAction )
                , ( "c", write closeBufferAndWindowE )
                , ( "q", write quitEditor )
                ]
  yesAction   = do fwriteBufferE (bkey firstBuffer)
                   withEditor closeBufferAndWindowE
                   askIndividualQuit others

  noAction    = do withEditor closeBufferAndWindowE
                   askIndividualQuit others

---------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are then simply confirms with the user that they
-- with to quit.
modifiedQuitEditor :: YiM ()
modifiedQuitEditor =
  do modifiedBuffers <- getModifiedBuffers
     if null modifiedBuffers
        then quitEditor
        else withEditor $ spawnMinibufferE modifiedMessage askKeymap >> return ()
  where
  modifiedMessage = "Modified buffers exist really quit? (y/n)"

  askKeymap       = const $ makeKeymap askBindings
  askBindings     = [ ("n", write noAction)
                    , ("y", write $ quitEditor)
                    ]

  noAction        = closeBufferAndWindowE

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> YiM ()
adjIndent ib = withSyntax (\m s -> modeIndent m s ib)


---------------------------
-- | Changing the buffer name quite useful if you have
-- several the same.

changeBufferNameE :: YiM ()
changeBufferNameE =
  withMinibufferGen "" noHint "New buffer name:" return strFun
  where
  strFun :: String -> YiM ()
  strFun = withBuffer . setnameB

----------------------------
-- | shell-command
shellCommandE :: YiM ()
shellCommandE = do
    withMinibufferGen "" noHint "Shell command:" return $ \cmd -> do
      (cmdOut,cmdErr,exitCode) <- liftIO $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> withEditor $ newBufferE "*Shell Command Output*" (LazyUTF8.fromString cmdOut) >> return ()
        -- FIXME: here we get a string and convert it back to utf8; this indicates a possible bug.
        ExitFailure _ -> msgEditor cmdErr

----------------------------

setupScript :: String
setupScript = "Setup"

----------------------------
-- cabal-configure
cabalConfigureE :: YiM ()
cabalConfigureE =
    withMinibuffer "Project directory:" (matchingFileNames Nothing) $ \fpath ->
    withMinibufferFree "Configure args:" $ \cmd -> do
      liftIO $ setCurrentDirectory fpath
      (cmdOut,cmdErr,exitCode) <- liftIO $ popen "runhaskell" (setupScript:"configure":words cmd) Nothing
      case exitCode of
        ExitSuccess   -> do withUI $ \ui -> reloadProject ui "."
                            withEditor $ withOtherWindow $ newBufferE "*Shell Command Output*" (fromString cmdOut) >> return ()
                            -- FIXME: here we get a string and convert it back to utf8; this indicates a possible bug.
        ExitFailure _ -> msgEditor cmdErr

reloadProjectE :: String -> YiM ()
reloadProjectE s = withUI $ \ui -> reloadProject ui s

----------------------------
-- cabal-build
cabalBuildE :: YiM ()
cabalBuildE =
    withMinibufferFree "Build args:" $ \cmd -> do
        startSubprocess "runhaskell" (setupScript:"build":words cmd)

-----------------------------
-- isearch
selfSearchKeymap :: Keymap
selfSearchKeymap = do
  Event (KASCII c) [] <- anyEvent
  write (isearchAddE [c])

searchKeymap :: Keymap
searchKeymap = selfSearchKeymap <|> makeKeymap
               [ -- ("C-g", isearchDelE) -- Only if string is not empty.
                 ("C-r", write isearchPrevE)
               , ("C-s", write isearchNextE)
               , ("C-w", write isearchWordE)
               , ("C-m", write $ isearchAddE "\n") -- I'm not sure this is the correct behaviour.
               , ("M-p", write $ isearchHistory 1)
               , ("M-n", write $ isearchHistory (-1))
               , ("BACKSP", write $ isearchDelE)
               ]

isearchKeymap :: Direction -> Keymap
isearchKeymap direction = 
  do write $ isearchInitE direction
     many searchKeymap
     makePartialKeymap [ ("C-g", write isearchCancelE)
                       , ("C-m", write isearchFinishE)
                       , ("RET", write isearchFinishE)
                       ]
                       (write isearchFinishE)

----------------------------
-- query-replace
queryReplaceE :: YiM ()
queryReplaceE = do
    withMinibufferFree "Replace:" $ \replaceWhat -> do
    withMinibufferFree "With:" $ \replaceWith -> do
    b <- withEditor $ getBuffer
    let replaceBindings = [("n", write $ qrNext b re),
                           ("y", write $ qrReplaceOne b re replaceWith),
                           ("q", write $ qrFinish),
                           ("C-g", write $ qrFinish)
                           ]
        Just re = makeSearchOptsM [] replaceWhat
    withEditor $ do
       setRegexE re
       spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q):")
            (const (makeKeymap replaceBindings))
       qrNext b re

executeExtendedCommandE :: YiM ()
executeExtendedCommandE = do
  withMinibuffer "M-x" (\_ -> getAllNamesInScope) execEditorAction

evalRegionE :: YiM ()
evalRegionE = do
  withBuffer (getSelectRegionB >>= readRegionB) >>= return -- FIXME: do something sensible.
  return ()

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents


insertSelf :: Char -> YiM ()
insertSelf = repeatingArg . insertB

insertNextC :: KeymapM ()
insertNextC = do c <- anyEvent
                 write $ repeatingArg $ insertB (eventToChar c)


-- Inserting a template from the templates defined in Yi.Templates.hs
insertTemplate :: YiM ()
insertTemplate =
  withMinibuffer "template-name:" (\_ -> return templateNames) $ withEditor . addTemplate

-- | C-u stuff
readArgC :: KeymapM ()
readArgC = do readArg' Nothing
              write $ do UniversalArg u <- withEditor getDynamic
                         logPutStrLn (show u)
                         msgEditor ""

-- TODO: This is crappy code: rewrite!
readArg' :: Maybe Int -> KeymapM ()
readArg' acc = do
    write $ msgEditor $ "Argument: " ++ show acc
    c <- anyEvent -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


-- | Open a file using the minibuffer. We have to set up some stuff to allow hints
--   and auto-completion.
findFile :: YiM ()
findFile = do maybePath <- withBuffer getfileB
              startPath <- addTrailingPathSeparator <$> (liftIO $ canonicalizePath' =<< getFolder maybePath)
              -- TODO: Just call withMinibuffer
              withMinibufferGen startPath (findFileHint startPath) "find file:" (simpleComplete $ matchingFileNames (Just startPath)) $ \filename -> do
                msgEditor $ "loading " ++ filename
                fnewE filename

-- | For use as the hint when opening a file using the minibuffer.
-- We essentially return all the files in the given directory which
-- have the given prefix.
findFileHint :: String -> String -> YiM String
findFileHint startPath s = 
  liftM (show . snd) $ getAppropriateFiles (Just startPath) s

-- | Given a possible starting path (which if not given defaults to
--   the current directory) and a fragment of a path we find all
--   files within the given (or current) directory which can complete
--   the given path fragment.
--   We return a pair of both directory plus the filenames on their own
--   that is without their directories. The reason for this is that if
--   we return all of the filenames then we get a 'hint' which is way too
--   long to be particularly useful.
getAppropriateFiles :: Maybe String -> String -> YiM (String, [ String ])
getAppropriateFiles start s = do
  curDir <- case start of
            Nothing -> do bufferPath <- withBuffer getfileB
                          liftIO $ getFolder bufferPath
            (Just path) -> return path
  let sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir
                  else if isAbsolute' sDir then sDir
                  else curDir </> sDir
  searchDir' <- liftIO $ expandTilda searchDir
  let fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir' </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- liftIO $ getDirectoryContents searchDir'
  -- Remove the two standard current-dir and parent-dir as we do not
  -- need to complete or hint about these as they are known by users.
  let files' = files \\ [ ".", ".." ]
  fs <- liftIO $ mapM fixTrailingPathSeparator files'
  let matching = filter (isPrefixOf $ takeFileName s) fs
  return (sDir, matching)


-- | Given a possible path and a prefix, return matching file names.
matchingFileNames :: Maybe String -> String -> YiM [String]
matchingFileNames start s = do
  (sDir, files) <- getAppropriateFiles start s
  return $ map (sDir </>) files

 
 -- | Given a path, trim the file name bit if it exists.  If no path
 --   given, return current directory.
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir



-- debug :: String -> Keymap
-- debug = write . logPutStrLn

-- | Returns all the buffer names.
matchingBufferNames :: String -> YiM [String]
matchingBufferNames _s = withEditor $ do
  bs <- getBuffers
  return (map name bs)

scrollDownE :: YiM ()
scrollDownE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> downScreenB
                 Just n -> replicateM_ n lineDown

scrollUpE :: YiM ()
scrollUpE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> upScreenB
                 Just n -> replicateM_ n lineUp

switchBufferE :: YiM ()
switchBufferE = do
  bs <- withEditor (map name . tail <$> getBufferStack)
  withMinibufferFin "switch to buffer:" bs (withEditor . switchToBufferWithNameE)


killBufferE :: YiM ()
killBufferE = withMinibuffer "kill buffer:" matchingBufferNames $ withEditor . closeBufferE

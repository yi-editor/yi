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
  , changeBufferNameE
  , rebind
  , withMinibuffer
  , queryReplaceE
  , isearchKeymap
  , shellCommandE
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
  , completeFileName
  , completeBufferName
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
import Control.Monad.Trans (MonadIO (..))

{- External Library Module Imports -}
{- Local (yi) module imports -}
import Yi.Yi
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Keys
import Yi.Buffer
import Yi.Process
import Yi.Editor
import Yi.Completion
import Yi.MiniBuffer
import Yi.Templates
  ( addTemplate
  , templateNames
  )

{- End of Module Imports -}

----------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are unmodified buffers then we ask individually for
-- each modified buffer whether or not the user wishes to save
-- it or not. If we get to the end of this list and there are still
-- some modified buffers then we ask again if the user wishes to
-- quit, but this is then a simple yes or no.
askQuitEditor :: YiM ()
askQuitEditor =
  do allBuffers      <- withEditor getBuffers
     modifiedBuffers <- filterM isFileBuffer $ filter (not . isUnchangedBuffer) allBuffers
     -- We could actually just call 'askIndividualQuit modifiedBuffers'
     -- here.
     if null modifiedBuffers
        then quitEditor
        else askIndividualQuit modifiedBuffers

-- | Is there a proper file associated with the buffer?
-- In other words, does it make sense to offer to save it?
isFileBuffer :: (Functor m, MonadIO m) => FBuffer -> m Bool
isFileBuffer b = case file b of
                   Nothing -> return False
                   Just fn -> not <$> liftIO (doesDirectoryExist fn)
                     
--------------------------------------------------
-- Takes in a list of buffers which have been identified
-- as modified since their last save.
askIndividualQuit :: [ FBuffer ] -> YiM ()
askIndividualQuit [] = modifiedQuitEditor
askIndividualQuit (firstBuffer : others) =
  spawnMinibufferE saveMessage askKeymap $ return ()
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
  yesAction   = do fwriteBufferE firstBuffer
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
  do allBuffers          <- withEditor getBuffers
     if any isUnchangedBuffer allBuffers
        then spawnMinibufferE modifiedMessage askKeymap $ return ()
        else quitEditor
  where
  modifiedMessage = "Modified buffers exist really quit? (y/n)"

  askKeymap       = const $ makeKeymap askBindings
  askBindings     = [ ("n", write noAction)
                    , ("y", write $ quitEditor)
                    ]

  noAction        = closeBufferAndWindowE



---------------------------
-- Changing the buffer name quite useful if you have
-- several the same.

changeBufferNameE :: YiM ()
changeBufferNameE =
  withMinibuffer "New buffer name:" return strFun
  where
  strFun :: String -> YiM ()
  strFun = withBuffer . setnameB

----------------------------
-- shell-command
shellCommandE :: YiM ()
shellCommandE = do
    withMinibuffer "Shell command:" return $ \cmd -> do
      (cmdOut,cmdErr,exitCode) <- liftIO $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> withEditor $ newBufferE "*Shell Command Output*" cmdOut >> return ()
        ExitFailure _ -> msgEditor cmdErr

-----------------------------
-- isearch
selfSearchKeymap :: Keymap
selfSearchKeymap = do
  Event (KASCII c) [] <- satisfy (const True)
  write (isearchAddE [c])

searchKeymap :: Keymap
searchKeymap = selfSearchKeymap <|> makeKeymap
               [ -- ("C-g", isearchDelE) -- Only if string is not empty.
                 ("C-r", write isearchPrevE)
               , ("C-s", write isearchNextE)
               , ("C-w", write isearchWordE)
               , ("C-n", write $ isearchAddE "\n")
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
    withMinibuffer "Replace:" return $ \replaceWhat -> do
    withMinibuffer "With:" return $ \replaceWith -> do
    b <- withEditor $ getBuffer
    let replaceBindings = [("n", write $ qrNext b replaceWhat),
                           ("y", write $ qrReplaceOne b replaceWhat replaceWith),
                           ("q", write $ closeBufferAndWindowE),
                           ("C-g", write $ closeBufferAndWindowE)
                           ]
    spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q):")
            (const (makeKeymap replaceBindings))
            (qrNext b replaceWhat)

executeExtendedCommandE :: YiM ()
executeExtendedCommandE = do
  withMinibuffer "M-x" completeFunctionName execEditorAction

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
insertNextC = do c <- satisfy (const True)
                 write $ repeatingArg $ insertB (eventToChar c)


-- Inserting a template from the templates defined in Yi.Templates.hs
insertTemplate :: YiM ()
insertTemplate =
  withMinibuffer "template-name:" completeTemplateName $ withEditor . addTemplate
  where
  completeTemplateName :: String -> YiM String
  completeTemplateName s = withEditor $ completeInList s (isPrefixOf s) templateNames

-- | C-u stuff
readArgC :: KeymapM ()
readArgC = do readArg' Nothing
              write $ do UniversalArg u <- withEditor getDynamic
                         logPutStrLn (show u)
                         msgEditor ""

readArg' :: Maybe Int -> KeymapM ()
readArg' acc = do
    write $ msgEditor $ "Argument: " ++ show acc
    c <- satisfy (const True) -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


findFile :: YiM ()
findFile = do maybePath <- withBuffer getfileB
              startPath <- liftIO $ getFolder maybePath
              withMinibuffer "find file:" (completeFileName (Just startPath)) $ \filename -> do {
               let filename' = fixFilePath startPath filename
             ; msgEditor $ "loading " ++ filename'
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



-- debug :: String -> Keymap
-- debug = write . logPutStrLn

completeBufferName :: String -> YiM String
completeBufferName s = withEditor $ do
  bs <- getBuffers
  completeInList s (isPrefixOf s) (map name bs)

completeFileName :: Maybe String -> String -> YiM String
completeFileName start s0 = do
  curDir <- case start of
            Nothing -> do bufferPath <- withBuffer getfileB
                          liftIO $ getFolder bufferPath
            (Just path) -> return path
  homeDir <- liftIO $ getHomeDirectory
  let s = if (['~',pathSeparator] `isPrefixOf` s0) then addTrailingPathSeparator homeDir ++ drop 2 s0 else s0
      sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir
                  else if isAbsolute sDir then sDir
                  else curDir </> sDir
      fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- liftIO $ getDirectoryContents searchDir
  let files' = files \\ [".", ".."]
  fs <- liftIO $ mapM fixTrailingPathSeparator files'
  withEditor $ completeInList s (isPrefixOf s) $ map (sDir </>) fs

completeFunctionName :: String -> YiM String
completeFunctionName s = do
  names <- getAllNamesInScope
  withEditor $ completeInList s (isPrefixOf s) names

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
switchBufferE = withMinibuffer "switch to buffer:" completeBufferName $ withEditor . switchToBufferWithNameE

killBufferE :: YiM ()
killBufferE = withMinibuffer "kill buffer:" completeBufferName $ withEditor . closeBufferE

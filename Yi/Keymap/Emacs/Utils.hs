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
  , changeBufferNameE
  , rebind
  , withMinibuffer
  , atomic
  , queryReplaceE
  , isearchKeymap
  , shellCommandE
  , executeExtendedCommandE
  , evalRegionE
  , readArgC
  , gotoLineE
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
import Control.Monad.Trans
  ( lift
  , liftIO
  )
import Data.Char
  ( ord
  , isDigit
  , isSpace
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

{- External Library Module Imports -}
{- Local (yi) module imports -}
import Yi.Yi
import Yi.Accessor
import Yi.Keymap.Emacs.KillRing
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
      (cmdOut,cmdErr,exitCode) <- lift $ runShellCommand cmd
      case exitCode of
        ExitSuccess -> newBufferE "*Shell Command Output*" cmdOut >> return ()
        ExitFailure _ -> msgE cmdErr

-----------------------------
-- isearch
selfSearchKeymap :: Keymap
selfSearchKeymap = do
  Event (KASCII c) [] <- satisfy (const True)
  write (isearchAddE [c])

searchKeymap :: Keymap
searchKeymap = selfSearchKeymap <|> makeKeymap
               [--("C-g", isearchDelE), -- Only if string is not empty.
                ("C-r", write isearchPrevE),
                ("C-s", write isearchNextE),
                ("C-w", write isearchWordE),
                ("BACKSP", write $ isearchDelE)]

isearchKeymap :: Direction -> Keymap
isearchKeymap direction = do
  write $ isearchInitE direction
  many searchKeymap
  foldr1 (<||) [events (readKey "C-g") >> write isearchCancelE,
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
                           ("q", write $ closeBufferAndWindowE),
                           ("C-g", write $ closeBufferAndWindowE)
                           ]
    spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q):")
            (const (makeKeymap replaceBindings))
            (qrNextE b replaceWhat)

executeExtendedCommandE :: YiM ()
executeExtendedCommandE = do
  withMinibuffer "M-x" completeFunctionName execE

evalRegionE :: YiM ()
evalRegionE = do
  withBuffer (getSelectRegionB >>= readRegionB) >>= evalE

-- | Define an atomic interactive command.
-- Purose is to define "transactional" boundaries for killring, undo, etc.
atomic :: (Show x, YiAction a x) => a -> KeymapM ()
atomic cmd = write $ do runAction (makeAction cmd)
                        withEditor $ modifyA killringA krEndCmd

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
                         msgE ""

readArg' :: Maybe Int -> KeymapM ()
readArg' acc = do
    write $ msgE $ "Argument: " ++ show acc
    c <- satisfy (const True) -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


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
{-# DEPRECATED gotoLineE "This is not necessary for Emacs keymap; should be moved to contrib" #-}
gotoLineE :: YiM ()
gotoLineE =
  withMinibuffer "Go to line:" return gotoAction
  where
  gotoAction :: String -> YiM ()
  gotoAction s =
    case parseLineAndChar s of
      Nothing     -> msgE "line and column number parse error"
      -- considering putting "gotoLineAndCol :: Int -> Int -> BufferM ()
      -- into Buffer.hs
      Just (l, c) -> withBuffer $ do gotoLn l
                                     rightN c

  -- This is actually relatively forgiving, for example "10.23xyh" will still
  -- take you to line number 10 column number 23
  -- in fact you can have any non digit character as the separator eg
  -- "10:24" or "10 23"
  -- In fact it need not be one character that is the separator, for example
  -- you can have: "3 my giddy aunt 43" and this will take you to line 3
  -- column 43.
  parseLineAndChar :: String -> Maybe (Int, Int)
  parseLineAndChar s
    | null lineString         = Nothing
    | null colString          = Just (read lineString, 0)
    | otherwise               = Just (read lineString, read colString)
    where
    (lineString, rest) = break (not . isDigit) $ dropWhile isSpace s
    colString          = takeWhile isDigit $ dropWhile (not . isDigit) rest

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
  let files' = files \\ [".", ".."]
  fs <- lift $ mapM fixTrailingPathSeparator files'
  withEditor $ completeInList s (isPrefixOf s) $ map (sDir </>) fs

completeFunctionName :: String -> YiM String
completeFunctionName s = do
  names <- getNamesInScopeE
  withEditor $ completeInList s (isPrefixOf s) names

scrollDownE :: YiM ()
scrollDownE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> downScreenE
                 Just n -> replicateM_ n lineDown

scrollUpE :: YiM ()
scrollUpE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> upScreenE
                 Just n -> replicateM_ n lineUp

switchBufferE :: YiM ()
switchBufferE = withMinibuffer "switch to buffer:" completeBufferName $ withEditor . switchToBufferWithNameE

killBufferE :: YiM ()
killBufferE = withMinibuffer "kill buffer:" completeBufferName $ withEditor . closeBufferE

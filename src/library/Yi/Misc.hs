{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Misc
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Various high-level functions to further classify.

module Yi.Misc ( getAppropriateFiles, getFolder, cd, pwd, matchingFileNames
               , rot13Char, placeMark, selectAll, adjBlock, adjIndent
               , promptFile , promptFileChangingHints, matchFile, completeFile
               , printFileInfoE, debugBufferContent
               ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (filterM, (>=>))
import           Control.Monad.Base      (liftBase)
import           Data.Char               (chr, isAlpha, isLower, isUpper, ord)
import           Data.List               ((\\))
import           Data.Maybe              (isNothing)
import qualified Data.Text               as T (Text, append, concat, isPrefixOf,
                                               pack, stripPrefix, unpack)
import           System.CanonicalizePath (canonicalizePath, replaceShorthands, replaceShorthands)
import           System.Directory        (doesDirectoryExist,
                                          getCurrentDirectory,
                                          getDirectoryContents,
                                          setCurrentDirectory)
import           System.Environment      (lookupEnv)
import           System.FilePath         (addTrailingPathSeparator,
                                          hasTrailingPathSeparator,
                                          takeDirectory, takeFileName, (</>))
import           System.FriendlyPath     (expandTilda, isAbsolute')
import           Yi.Buffer
import           Yi.Completion           (completeInList')
import           Yi.Editor               (EditorM, printMsg, withCurrentBuffer)
import           Yi.Keymap               (YiM)
import           Yi.MiniBuffer           (debugBufferContent, mkCompleteFn, withMinibufferGen)
import           Yi.Monad                (gets)
import qualified Yi.Rope                 as R (fromText)
import           Yi.Utils                (io)

-- | Given a possible starting path (which if not given defaults to
-- the current directory) and a fragment of a path we find all files
-- within the given (or current) directory which can complete the
-- given path fragment. We return a pair of both directory plus the
-- filenames on their own that is without their directories. The
-- reason for this is that if we return all of the filenames then we
-- get a 'hint' which is way too long to be particularly useful.
getAppropriateFiles :: Maybe T.Text -> T.Text -> YiM (T.Text, [ T.Text ])
getAppropriateFiles start s' = do
  curDir <- case start of
    Nothing -> do bufferPath <- withCurrentBuffer $ gets file
                  liftBase $ getFolder bufferPath
    Just path -> return $ T.unpack path
  let s = T.unpack $ replaceShorthands s'
      sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir
        | null sDir = curDir
        | isAbsolute' sDir = sDir
        | otherwise = curDir </> sDir
  searchDir' <- liftBase $ expandTilda searchDir
  let fixTrailingPathSeparator f = do
        isDir <- doesDirectoryExist (searchDir' </> f)
        return . T.pack $ if isDir then addTrailingPathSeparator f else f

  files <- liftBase $ getDirectoryContents searchDir'

  -- Remove the two standard current-dir and parent-dir as we do not
  -- need to complete or hint about these as they are known by users.
  let files' = files \\ [ ".", ".." ]
  fs <- liftBase $ mapM fixTrailingPathSeparator files'
  let matching = filter (T.isPrefixOf . T.pack $ takeFileName s) fs
  return (T.pack sDir, matching)

-- | Given a path, trim the file name bit if it exists.  If no path
--   given, return current directory.
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir


-- | Given a possible path and a prefix, return matching file names.
matchingFileNames :: Maybe T.Text -> T.Text -> YiM [T.Text]
matchingFileNames start s = do
  (sDir, files) <- getAppropriateFiles start s

  -- There is one common case when we don't need to prepend @sDir@ to @files@:
  --
  -- Suppose user just wants to edit a file "foobar" in current directory
  -- and inputs ":e foo<Tab>"
  --
  -- @sDir@ in this case equals to "." and "foo" would not be
  -- a prefix of ("." </> "foobar"), resulting in a failed completion
  --
  -- However, if user inputs ":e ./foo<Tab>", we need to prepend @sDir@ to @files@
  let results = if isNothing start && sDir == "." && not ("./" `T.isPrefixOf` s)
                   then files
                   else fmap (T.pack . (T.unpack sDir </>) . T.unpack) files

  return results

-- | Place mark at current point. If there's an existing mark at point
-- already, deactivate mark.
placeMark :: BufferM ()
placeMark = (==) <$> pointB <*> getSelectionMarkPointB >>= \case
  True -> setVisibleSelection False
  False -> setVisibleSelection True >> pointB >>= setSelectionMarkPointB

-- | Select the contents of the whole buffer
selectAll :: BufferM ()
selectAll = botB >> placeMark >> topB >> setVisibleSelection True

adjBlock :: Int -> BufferM ()
adjBlock x = withSyntaxB' (\m s -> modeAdjustBlock m s x)

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> BufferM ()
adjIndent ib = withSyntaxB' (\m s -> modeIndent m s ib)

-- | Generic emacs style prompt file action. Takes a @prompt@ and a continuation
-- @act@ and prompts the user with file hints.
promptFile :: T.Text -> (T.Text -> YiM ()) -> YiM ()
promptFile prompt act = promptFileChangingHints prompt (const return) act

-- | As 'promptFile' but additionally allows the caller to transform
-- the list of hints arbitrarily, such as only showing directories.
promptFileChangingHints :: T.Text -- ^ Prompt
                        -> (T.Text -> [T.Text] -> YiM [T.Text])
                        -- ^ Hint transformer: current path, generated hints
                        -> (T.Text -> YiM ()) -- ^ Action over choice
                        -> YiM ()
promptFileChangingHints prompt ht act = do
  maybePath <- withCurrentBuffer $ gets file
  startPath <- T.pack . addTrailingPathSeparator
               <$> liftBase (canonicalizePath =<< getFolder maybePath)
  -- TODO: Just call withMinibuffer
  withMinibufferGen startPath (\x -> findFileHint startPath x >>= ht x) prompt
    (completeFile startPath) showCanon (act . replaceShorthands)
  where
    showCanon = withCurrentBuffer . replaceBufferContent . R.fromText . replaceShorthands

matchFile :: T.Text -> T.Text -> Maybe T.Text
matchFile path proposedCompletion =
  let realPath = replaceShorthands path
  in T.append path <$> T.stripPrefix realPath proposedCompletion

completeFile :: T.Text -> T.Text -> YiM T.Text
completeFile startPath =
  mkCompleteFn completeInList' matchFile $ matchingFileNames (Just startPath)

-- | For use as the hint when opening a file using the minibuffer. We
-- essentially return all the files in the given directory which have
-- the given prefix.
findFileHint :: T.Text -> T.Text -> YiM [T.Text]
findFileHint startPath s = snd <$> getAppropriateFiles (Just startPath) s

onCharLetterCode :: (Int -> Int) -> Char -> Char
onCharLetterCode f c | isAlpha c = chr (f (ord c - a) `mod` 26 + a)
                     | otherwise = c
                     where a | isUpper c = ord 'A'
                             | isLower c = ord 'a'
                             | otherwise = undefined

-- | Like @M-x cd@, it changes the current working directory. Mighty
-- useful when we don't start Yi from the project directory or want to
-- switch projects, as many tools only use the current working
-- directory.
cd :: YiM ()
cd = promptFileChangingHints "switch directory to:" dirs $ \path ->
  io $ getFolder (Just $ T.unpack path) >>= clean . T.pack
       >>= System.Directory.setCurrentDirectory . addTrailingPathSeparator
  where
     replaceHome p@('~':'/':xs) = lookupEnv "HOME" >>= return . \case
       Nothing -> p
       Just h -> h </> xs
     replaceHome p = return p
     clean = replaceHome . T.unpack . replaceShorthands >=> canonicalizePath

     x <//> y = T.pack $ takeDirectory (T.unpack x) </> T.unpack y

     dirs :: T.Text -> [T.Text] -> YiM [T.Text]
     dirs x xs = do
       xsc <- io $ mapM (\y -> (,y) <$> clean (x <//> y)) xs
       filterM (io . doesDirectoryExist . fst) xsc >>= return . map snd

-- | Shows current working directory. Also see 'cd'.
pwd :: YiM ()
pwd = io getCurrentDirectory >>= printMsg . T.pack

rot13Char :: Char -> Char
rot13Char = onCharLetterCode (+13)

printFileInfoE :: EditorM ()
printFileInfoE = printMsg . showBufInfo =<< withCurrentBuffer bufInfoB
    where showBufInfo :: BufferFileInfo -> T.Text
          showBufInfo bufInfo = T.concat
            [ T.pack $ bufInfoFileName bufInfo
            , " Line "
            , T.pack . show $ bufInfoLineNo bufInfo
            , " ["
            , bufInfoPercent bufInfo
            , "]"
            ]

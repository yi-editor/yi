module System.FriendlyPath where
import System.FilePath
import System.Directory
import Data.List


-- | A version of canonicalizePath that works.

-- Note that we cannot use canonicalizePath because if used on a non existing directory
-- then the file part will be dropped (arguably this is a bug in canonicalizePath)
-- eg. @x/y@ can become @x@, and we do not want that.
canonicalizePathFix :: FilePath -> IO FilePath
canonicalizePathFix f = do
    de <- doesFileExist f
    if de then canonicalizePath f else makeAbsolute f


-- The documentation for 'System.FilePath.makeRelative' is wrong. It says

-- There is no corresponding makeAbsolute function, instead use
-- System.Directory.canonicalizePath which has the same effect.

-- canonicalizePath follows symlinks, and does not work if the directory does not exist.

-- | Make a path absolute.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute f
    | isAbsolute' f = return f
    | otherwise = fmap (</> f) getCurrentDirectory 


-- | Canonicalize a user-friendly path
userToCanonPath :: FilePath -> IO String
userToCanonPath f = canonicalizePathFix =<< expandTilda f


-- | Make a path more user-friendly by replacing the home directory with tilda.
recoverTilda :: FilePath -> IO String
recoverTilda path = do
  home <- getHomeDirectory
  return $ if home `isPrefixOf` path
    then "~" ++ drop (length home) path
    else path

-- | Turn a path into its canonicalized, user-friendly version.
canonicalizePath' :: FilePath -> IO String
canonicalizePath' f = recoverTilda =<< canonicalizePathFix f

-- | Turn a user-friendly path into a computer-friendly path by expanding the leading tilda.
expandTilda :: String -> IO FilePath
expandTilda ('~':path)
  | (null path) || (head path == pathSeparator) = (++ path) <$> getHomeDirectory
  -- Home directory of another user, e.g. ~root/
  | otherwise = let username = takeWhile (/= pathSeparator) path
                    dirname = drop (length username) path
                in  (normalise . (++ dirname) . homeDirectory) <$> getUserEntryForName username
expandTilda path = return path

-- | Is a user-friendly path absolute?
isAbsolute' :: String -> Bool
isAbsolute' ('~':_) = True
isAbsolute' p = isAbsolute p



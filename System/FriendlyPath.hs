module System.FriendlyPath where
import System.FilePath
import System.Directory
import Data.List

-- | Make a path more user-friendly by replacing the home directory with tilda.
recoverTilda :: FilePath -> IO String
recoverTilda path = do
  home <- getHomeDirectory
  return $ if home `isPrefixOf` path
    then "~" ++ drop (length home) path
    else path

-- | Turn a path into its canonicalized, user-friendly version.
canonicalizePath' :: FilePath -> IO String
canonicalizePath' f = recoverTilda =<< canonicalizePath f

-- | Turn a user-friendly path into a computer-friendly path by expanding the leading tilda.
expandTilda :: String -> IO FilePath
expandTilda s0 = do
  home <- getHomeDirectory
  return $ if (['~',pathSeparator] `isPrefixOf` s0) then home </> drop 2 s0 else s0

-- | Is a user-friendly path absolute?
isAbsolute' :: String -> Bool
isAbsolute' ('~':_) = True
isAbsolute' p = isAbsolute p



module System.FriendlyPath
  ( userToCanonPath
  , expandTilda
  , isAbsolute'
  ) where

import Control.Applicative     ((<$>))
import System.CanonicalizePath (canonicalizePath)
import System.Directory        (getHomeDirectory)
import System.FilePath         (isAbsolute, normalise, pathSeparator)
import System.PosixCompat.User (getUserEntryForName, homeDirectory)


-- canonicalizePath follows symlinks, and does not work if the directory does not exist.

-- | Canonicalize a user-friendly path
userToCanonPath :: FilePath -> IO String
userToCanonPath f = canonicalizePath =<< expandTilda f

-- | Turn a user-friendly path into a computer-friendly path by expanding the leading tilda.
expandTilda :: String -> IO FilePath
expandTilda ('~':path)
  | null path || (head path == pathSeparator) = (++ path) <$> getHomeDirectory
  -- Home directory of another user, e.g. ~root/
  | otherwise = let username = takeWhile (/= pathSeparator) path
                    dirname = drop (length username) path
                in  (normalise . (++ dirname) . homeDirectory) <$> getUserEntryForName username
expandTilda path = return path

-- | Is a user-friendly path absolute?
isAbsolute' :: String -> Bool
isAbsolute' ('~':_) = True
isAbsolute' p = isAbsolute p



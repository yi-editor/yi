-- | Utility functions/instances for HConf for which a better place has not
-- been found yet.
module HConf.Utils (shellWords, (+++), io) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (unfoldr)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))

(+++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
Nothing +++ x = x
x +++ Nothing = x
(Just x) +++ (Just y) = Just (x ++ y)

-- | Lift an IO action
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Break up a string the way a shell breaks up a command into arguments.
-- Similar to 'words', but respects quotes and escaped spaces.  TODO: Verify
-- this function.

shellWords :: String -> [String]
shellWords = filter (not.null) . unfoldr maybeLex

-- Helper functions for shellWords

maybeLex :: String -> Maybe (String,String)
maybeLex "" = Nothing
maybeLex s  = Just (shellLex s)

shellLex :: String -> (String,String)
shellLex ""          = ("","")
shellLex (' ':s)     = ("",s)
shellLex (c:s) | c `elem` ['\'','"']
                     = quotes c s
shellLex ('\\':x:s)  = (x:lexed,rest)       where (lexed,rest) = shellLex s
shellLex (x:s)       = (x:lexed,rest)       where (lexed,rest) = shellLex s

quotes :: Char -> String -> (String,String)
quotes _ ""             = ("","")  -- Unterminated quote (an error really)
quotes c (x:s) | x == c = ("",s)
quotes c ('\\':s)       = backslash c s
quotes c (x:s)          = (x:lexed,rest) where (lexed,rest) = quotes c s

backslash :: Char -> String -> (String,String)
backslash _ ""                = ("\\","") -- Trailing backslash?
backslash c (x:s) | c == x    = (x:lexed, rest)
                  | otherwise = ('\\' : x : lexed, rest)
                    where (lexed,rest) = quotes c s

-- Functor instances for data types from System.Console.GetOpt

instance Functor ArgDescr where
    fmap f (NoArg x)    = NoArg (f x)
    fmap f (ReqArg x s) = ReqArg (f . x) s
    fmap f (OptArg x s) = OptArg (f . x) s

instance Functor OptDescr where
    fmap f (Option short long argdescr usage) =
            Option short long (fmap f argdescr) usage


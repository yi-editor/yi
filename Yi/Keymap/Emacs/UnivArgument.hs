{-# OPTIONS -cpp -fglasgow-exts #-}
--
-- Copyright (c) 2005 Jean-Philippe Bernardy
--
--
module Yi.Keymap.Emacs.UnivArgument where

import Yi.Core
import Yi.Keymap

import Data.Maybe
import Data.Dynamic

import Control.Monad ( replicateM_ )

newtype UniversalArg = UniversalArg (Maybe Int)
    deriving Typeable

-- doing the argument precisely is kind of tedious.
-- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
-- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html

instance Initializable UniversalArg where
    initial = UniversalArg Nothing


withUnivArg :: YiAction m => (Maybe Int -> m ()) -> YiM ()
withUnivArg cmd = do UniversalArg a <- getDynamic
                     runAction $ makeAction (cmd a)
                     setDynamic $ UniversalArg Nothing

withIntArg :: YiAction m => (Int -> m ()) -> YiM ()
withIntArg cmd = withUnivArg $ \arg -> cmd (fromMaybe 1 arg)

repeatingArg :: (Monad m, YiAction m) => m () -> YiM ()
repeatingArg f = withIntArg $ \n -> replicateM_ n f

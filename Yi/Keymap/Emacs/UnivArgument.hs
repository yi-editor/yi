{-# OPTIONS -cpp -fglasgow-exts #-}
-- 
-- Copyright (c) 2005 Jean-Philippe Bernardy
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
module Yi.Keymap.Emacs.UnivArgument where

import Yi.Editor
import Yi.Core

import Data.Maybe
import Data.Dynamic

import Control.Monad ( replicateM_ )

newtype UniversalArg = UniversalArg (Maybe Int)
    deriving Typeable

-- doing the argument precisely is kind of tedious.
-- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
-- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html

instance Initializable UniversalArg where
    initial = return $ UniversalArg Nothing

withUnivArg :: (Maybe Int -> Action) -> Action
withUnivArg cmd = do UniversalArg a <- getDynamic
                     cmd a
                     setDynamic $ UniversalArg Nothing

withIntArg :: (Int -> Action) -> Action
withIntArg cmd = withUnivArg $ \arg -> cmd (fromMaybe 1 arg)

repeatingArg :: Action -> Action
repeatingArg f = withIntArg $ \n->replicateM_ n f

{-# OPTIONS -#include "YiUtils.h" #-}
--
-- Copyright (C) 2007 Stefan O'Rear
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

-- | This module defines the list of syntax highlighter modules.
--

module Yi.Syntax.Table ( ExtHL(..), highlighters ) where

import Yi.Syntax
import qualified Yi.Syntax.Haskell

import qualified Data.Map as M

data ExtHL = forall a. Eq a => ExtHL (Maybe (Highlighter a))

highlighters :: M.Map String ExtHL
highlighters = M.fromList [ ( "haskell", ExtHL (Just Yi.Syntax.Haskell.highlighter) )
                          , ( "none",    ExtHL (Nothing :: Maybe (Highlighter ())) )
                          ]

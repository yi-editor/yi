--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

--
-- Syntax highlighting api
--

module Yi.Syntax.Syntax (
        Syntax(..),
        Drawable(..),

        StyleTable,
        emptySty,
        insertSty,
        lookupSty,
    ) where

import Yi.Buffer
import Yi.Style

import Yi.Map (Map)
import qualified Yi.Map as M hiding (Map)

--
-- The Syntax type is instansiated for each abstract syntax type.
--
class Syntax a where

    synname     :: a -> String                  -- e.g. "Haskell"

    -- need to go from buffer names to valid extensions
    extensions  :: a -> [String]                -- e.g. [.lhs, .hs]

    -- | Parser a buffer into an abstract syntax tree
    parse       :: Buffer b => b -> IO a        -- how to parse

    -- | Walk an abstract syntax tree  (hmm. is this type at all sensible?)
    foldSyn     :: (b -> a -> b) -> b -> a -> b -- how to traverse

--
-- known parsers need to be registered with the Editor.
-- parser threads need to be registered too, and shut down on closing
-- the buffer.
--

------------------------------------------------------------------------
--
-- when rendering a buffer we walk the syn tree constructing a table
-- mapping buffer indicies to styles.
--

type StyleTable = Map Int Style

emptySty :: StyleTable
emptySty = M.empty

insertSty :: StyleTable -> Int -> Style -> StyleTable
insertSty = \m k e -> M.insert k e m

lookupSty :: StyleTable -> Int -> Maybe Style
lookupSty = flip M.lookup

------------------------------------------------------------------------

--
-- An element in a Syntax tree is Drawable if we can retrieve it's
-- position, and find it's colouring.
--
class Drawable a where
    -- | given an abstract syntax item, extract its style
    styleOf :: a -> Style

    -- | given an abstract syntax item, extract its source location range
    posOf   :: a -> (Int,Int)

-- | Flatten a syn tree back into a list of located tokens
-- Not sure what I was trying to do here.
-- synToList :: Syntax a => a -> StyleTable
-- synToList abssyn = foldSyn (\b a -> insertSty b a (styleOf b)) emptySty abssyn

------------------------------------------------------------------------

{-
instance Syntax HsSyn where
    name        = "haskell"
    extensions  = [ ".hs" ]
    parser      = Yi.Syntax.Haskell.parser
-}


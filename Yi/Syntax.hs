{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
--
-- Copyright (C) 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

-- | This module defines a common interface for syntax highlighters.
--
-- Yi syntax highlighters are expressed as explicit lazy computations
-- of type 'Highlighter a' below; this type is effectively isomorphic
-- to [Char] -> [Style], but are explicitly lazy to admit safe fast uses.
--

module Yi.Syntax 
  ( Highlighter  ( .. )
  , ExtHL        ( .. )
  , noHighlighter
  , Point, Size, Stroke
  ) 
where

import Yi.Style
import qualified Data.ByteString.Lazy.Char8 as LB

type Point = Int
type Size = Int
type Stroke = (Point,Style,Point)

-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

data Highlighter a = 
  SynHL { hlStartState :: a -- ^ The start state for the highlighter.
        , hlRun :: LB.ByteString -> (Int,a) -> ([(Int,a)], [Stroke])
        }

noHighlighter :: Highlighter ()
noHighlighter = SynHL {hlStartState = (), 
                       hlRun = \_ _ -> ([],[])}

data ExtHL = forall a. ExtHL (Highlighter a)

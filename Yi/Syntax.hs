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
  , Highlighter', withScanner
  , Scanner (..)
  , ExtHL        ( .. )
  , noHighlighter
  , Point, Size, Stroke
  ) 
where

import Yi.Style

type Point = Int
type Size = Int
type Stroke = (Point,Style,Point)


-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

-- FIXME: this actually does more than just HL, so the names are silly.
-- FIXME: "State" is actually CacheState

type Highlighter' = Highlighter Int Char

data Highlighter st tok a = 
  SynHL { hlStartState :: a -- ^ The start state for the highlighter.
        , hlRun :: Scanner st tok -> Int -> a -> a
        , hlGetStrokes :: Int -> Int -> a -> [Stroke]
        }

data Scanner st a = Scanner {
--                             stStart :: st -> Int,
--                             stLooked :: st -> Int,
                             scanInit :: st,
                             scanRun  :: st -> [(st,a)]}


withScanner :: (Scanner s1 t1 -> Scanner s2 t2) -> Highlighter s2 t2 a -> Highlighter s1 t1 a
withScanner f (SynHL a r gs) = SynHL a (\scanner i a -> r (f scanner) i a) gs

noHighlighter :: Highlighter' ()
noHighlighter = SynHL {hlStartState = (), 
                       hlRun = \_ _ a -> a,
                       hlGetStrokes = \_ _ _ -> []}

data ExtHL = forall a. ExtHL (Highlighter' a)

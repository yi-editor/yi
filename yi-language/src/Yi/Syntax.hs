{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Syntax
-- Copyright   :  (c) Don Stewart 2007
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a common interface for syntax-awareness.
--
-- There have been many tens of wasted hours in this and lexer
-- modules. This note is to commemorate those who have fallen in
-- battle.

module Yi.Syntax
  ( Highlighter  ( .. )
  , Cache
  , Scanner (..)
  , ExtHL        ( .. )
  , noHighlighter, mkHighlighter, skipScanner, emptyFileScan
  , Point(..), Size(..), Length, Stroke
  , Span(..)
  )
where

import qualified  Data.Map as M
import Control.Arrow
import Yi.Style
import Data.Foldable
import Data.Traversable
import Yi.Buffer.Basic
import Yi.Region

type Length = Int                   -- size in #codepoints

type Stroke = Span StyleName
data Span a = Span {spanBegin :: !Point, spanContents :: !a, spanEnd :: !Point}
    deriving (Show, Functor, Foldable, Traversable)

-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

-- FIXME: this is actually completetly abstrcted from sytnax HL, so
-- the names are silly.

data Highlighter cache syntax =
  SynHL { hlStartState :: cache -- ^ The start state for the highlighter.
        , hlRun :: Scanner Point Char -> Point -> cache -> cache
        , hlGetTree :: cache -> WindowRef -> syntax
        , hlFocus :: M.Map WindowRef Region -> cache -> cache
        -- ^ focus at a given point, and return the coresponding node.
        -- (hint -- the root can always be returned, at the cost of
        -- performance.)
        }

data ExtHL syntax = forall cache. ExtHL (Highlighter cache syntax)

data Scanner st a = Scanner
  { scanInit :: st -- ^ Initial state
  , scanLooked :: st -> Point
    -- ^ How far did the scanner look to produce this intermediate state?
    -- The state can be reused as long as nothing changes before that point.
  , scanEmpty :: a      --  hack :/
  , scanRun  :: st -> [(st ,a)]
    -- ^ Running function returns a list of results and intermediate
    -- states. Note: the state is the state /before/ producing the
    -- result in the second component.
  }

skipScanner :: Int -> Scanner st a -> Scanner st a
skipScanner n (Scanner i l e r) = Scanner i l e (other 0 . r)
    where
      other _ [] = []
      other _ [x] = [x] -- we must return the final result (because if
                        -- the list is empty mkHighlighter thinks it
                        -- can reuse the previous result)
      other 0 (x:xs) = x : other n xs
      other m (_:xs) = other (m-1) xs

instance Functor (Scanner st) where
    fmap f (Scanner i l e r) = Scanner i l (f e) (fmap (second f) . r)

data Cache state result = Cache [state] result

emptyFileScan :: Scanner Point Char
emptyFileScan = Scanner
  { scanInit = 0
  , scanRun = const []
  , scanLooked = id
  , scanEmpty = error "emptyFileScan: no scanEmpty"
  }

-- | This takes as input a scanner that returns the "full" result at
-- each element in the list; perhaps in a different form for the
-- purpose of incremental-lazy eval.
mkHighlighter :: forall state result. Show state =>
                 (Scanner Point Char -> Scanner state result) ->
                     Highlighter (Cache state result) result
mkHighlighter scanner =
  Yi.Syntax.SynHL
        { hlStartState   = Cache [] emptyResult
        , hlRun          = updateCache
        , hlGetTree      = \(Cache _ result) _windowRef -> result
        , hlFocus        = \_ c -> c
        }
    where startState :: state
          startState = scanInit    (scanner emptyFileScan)
          emptyResult = scanEmpty (scanner emptyFileScan)
          updateCache :: Scanner Point Char -> Point -> Cache state result -> Cache state result
          updateCache newFileScan dirtyOffset (Cache cachedStates oldResult) = Cache newCachedStates newResult
            where newScan = scanner newFileScan
                  reused :: [state]
                  reused = takeWhile ((< dirtyOffset) . scanLooked (scanner newFileScan)) cachedStates
                  resumeState :: state
                  resumeState = if null reused then startState else last reused

                  newCachedStates = reused ++ fmap fst recomputed
                  recomputed = scanRun newScan resumeState
                  newResult :: result
                  newResult = if null recomputed then oldResult else snd $ head recomputed

noHighlighter :: Highlighter () syntax
noHighlighter = SynHL
  { hlStartState = ()
  , hlRun = \_ _ a -> a
  , hlFocus = \_ c -> c
  , hlGetTree = \ _ -> error "noHighlighter: tried to use syntax"
  }

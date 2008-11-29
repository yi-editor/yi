{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
--
-- Copyright (C) 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

-- | This module defines a common interface for syntax highlighters.

module Yi.Syntax 
  ( Highlighter  ( .. )
  , Cache
  , Scanner (..)
  , ExtHL        ( .. )
  , noHighlighter, mkHighlighter, skipScanner
  , Point(..), Size(..), Length, Stroke
  ) 
where

import Control.Arrow
import Yi.Style
import Yi.Prelude
import Prelude ()
import Data.List (takeWhile)
import Yi.Buffer.Basic

type Length = Int                   -- size in #codepoints

type Stroke = (Point,StyleName,Point)
-- TODO: use Region datatype.


-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

-- FIXME: this actually does more than just HL, so the names are silly.

data Highlighter cache syntax = 
  SynHL { hlStartState :: cache -- ^ The start state for the highlighter.
        , hlRun :: Scanner Point Char -> Point -> cache -> cache
        , hlGetStrokes :: Point -> Point -> Point -> syntax -> [Stroke]
         -- TODO: move hlGetStrokes out of this into the Buffer
        , hlGetTree :: cache -> syntax
        }

data ExtHL syntax = forall a. ExtHL (Highlighter a syntax) 

data Scanner st a = Scanner {
                             scanInit :: st, -- ^ Initial state
                             scanLooked :: st -> Point, 
                             -- ^ How far did the scanner look to produce this intermediate state?
                             -- The state can be reused as long as nothing changes before that point.
                             scanEmpty :: a,      --  hack :/
                             scanRun  :: st -> [(st,a)]  -- ^ Running function returns a list of returns and intermediate states.
                            }

skipScanner :: Int -> Scanner st a -> Scanner st a
skipScanner n (Scanner i l e r) = Scanner i l e (other n . r)
    where other _ [] = []
          other 0 (x:xs) = x : other n xs
          other m (_:xs) = other (m-1) xs

instance Functor (Scanner st) where
    fmap f (Scanner i l e r) = Scanner i l (f e) (\st -> fmap (second f) (r st))

data Cache state result = Cache [state] result

emptyFileScan :: Scanner Point Char
emptyFileScan = Scanner { scanInit = 0, scanRun = const [], scanLooked = id, scanEmpty = error "emptyFileScan: no scanEmpty" }

-- | This takes as input a scanner that returns the "full" result at
-- each element in the list; perhaps in a different form for the
-- purpose of incremental-lazy eval.
mkHighlighter :: forall state result. 
                 (Scanner Point Char -> Scanner state result) -> 
                 (Point -> Point -> Point -> result -> [Stroke]) -> 
                     Highlighter (Cache state result) result
mkHighlighter scanner getStrokes =
  Yi.Syntax.SynHL 
        { hlStartState   = Cache [] emptyResult
        , hlRun          = updateCache
        , hlGetStrokes   = getStrokes
        , hlGetTree      = \(Cache _ result) -> result
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
                  newResult = if null recomputed then oldResult else snd $ head $ recomputed

noHighlighter :: Highlighter () syntax
noHighlighter = SynHL {hlStartState = (), 
                       hlRun = \_ _ a -> a,
                       hlGetStrokes = \_ _ _ _ -> [],
                       hlGetTree = \_ -> error "noHighlighter: tried to fetch syntax"
                      }



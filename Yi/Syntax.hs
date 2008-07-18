{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
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
  , Scanner (..)
  , ExtHL        ( .. )
  , noHighlighter, mkHighlighter
  , Point(..), Size(..), Length, Stroke
  ) 
where

import Yi.Style
import Yi.Prelude
import Prelude ()
import Data.List (takeWhile)
import Yi.Buffer.Basic

type Length = Int                   -- size in #codepoints

type Stroke = (Point,Style,Point)
-- TODO: use Region datatype.


-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

-- FIXME: this actually does more than just HL, so the names are silly.

data Highlighter cache syntax = 
  SynHL { hlStartState :: cache -- ^ The start state for the highlighter.
        , hlRun :: Scanner Point Char -> Point -> cache -> cache
        , hlGetStrokes :: Point -> Point -> Point -> cache -> [Stroke]
        , hlGetTree :: cache -> syntax
        }

data Scanner st a = Scanner {
--                             scanStart :: st -> Int,
                             scanInit :: st, -- ^ Initial state
                             scanLooked :: st -> Point,
                             scanEmpty :: a,      --  hack :/
                             scanRun  :: st -> [(st,a)]  -- ^ Running function returns a list of returns and intermediate states.
                                         }
scanRunInit :: Scanner st a -> [(st, a)]
scanRunInit s = scanRun s $ scanInit s

data Cache state result = Cache [(state,result)] result

emptyFileScan = Scanner { scanInit = 0, scanRun = const [], scanLooked = id }

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
        , hlGetStrokes   = \begin end pos (Cache _ result) -> getStrokes begin end pos result
        , hlGetTree      = \(Cache _ result) -> result
        }
    where startState :: state
          startState = scanInit    (scanner emptyFileScan)
          emptyResult = scanEmpty (scanner emptyFileScan)
          updateCache :: Scanner Point Char -> Point -> Cache state result -> Cache state result
          updateCache newFileScan dirtyOffset (Cache cachedStates _) = Cache newCachedStates newResult
            where newScan = scanner newFileScan
                  reused :: [(state,result)]
                  reused = takeWhile ((< dirtyOffset) . scanLooked (scanner newFileScan) . fst) cachedStates
                  resumeState :: (state,result)
                  resumeState = if null reused then (startState, emptyResult) else last reused
                  newCachedStates = reused ++ recomputed
                  recomputed = scanRun newScan (fst resumeState)
                  newResult :: result
                  newResult = snd $ head (recomputed ++ [resumeState])

noHighlighter :: Highlighter () syntax
noHighlighter = SynHL {hlStartState = (), 
                       hlRun = \_ _ a -> a,
                       hlGetStrokes = \_ _ _ _ -> [],
                       hlGetTree = \_ -> error "noHighlighter: tried to fetch syntax"
                      }

data ExtHL syntax = forall a. ExtHL (Highlighter a syntax) 

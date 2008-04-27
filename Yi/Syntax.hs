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
  , Highlighter', withScanner
  , Scanner (..)
  , ExtHL        ( .. )
  , noHighlighter
  , Point(..), Size(..), Length, Stroke
  ) 
where

import Yi.Style
import Yi.Prelude
import Prelude ()
import Yi.Buffer.Basic

type Length = Int                   -- size in #codepoints

type Stroke = (Point,Style,Point)


-- | The main type of syntax highlighters.  This record type combines all
-- the required functions, and is parametrized on the type of the internal
-- state.

-- FIXME: this actually does more than just HL, so the names are silly.
-- FIXME: "State" is actually CacheState

type Highlighter' = Highlighter Point Char

data Highlighter st tok cache syntax = 
  SynHL { hlStartState :: cache -- ^ The start state for the highlighter.
        , hlRun :: Scanner st tok -> Point -> cache -> cache
        , hlGetStrokes :: Point -> Point -> Point -> cache -> [Stroke]
        , hlGetTree :: cache -> syntax
        }

data Scanner st a = Scanner {
--                             stStart :: st -> Int,
--                             stLooked :: st -> Int,
                             scanInit :: st,
                             scanRun  :: st -> [(st,a)]}


withScanner :: (Scanner s1 t1 -> Scanner s2 t2) -> Highlighter s2 t2 a syntax -> Highlighter s1 t1 a syntax
withScanner f (SynHL cache0 r gs gt) = SynHL cache0 (\scanner i cache -> r (f scanner) i cache) gs gt

noHighlighter :: Highlighter' () syntax
noHighlighter = SynHL {hlStartState = (), 
                       hlRun = \_ _ a -> a,
                       hlGetStrokes = \_ _ _ _ -> [],
                       hlGetTree = \_ -> error "noHighlighter: tried to fetch syntax"
                      }

data ExtHL syntax = forall a. ExtHL (Highlighter' a syntax) 

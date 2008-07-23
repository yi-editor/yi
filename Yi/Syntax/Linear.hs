-- Copyright (C) 2008 JP Bernardy

module Yi.Syntax.Linear
  ( getStrokes, incrScanner, Result ) 
where

import Control.Arrow
import Yi.Style
import Yi.Prelude
import Prelude ()
import Data.List (takeWhile, dropWhile, reverse)
import Yi.Buffer.Basic
import Yi.Syntax


data Result tok = Result [tok] [tok]

instance Functor Result where
    fmap f (Result a b) = Result (fmap f a) (fmap f b)

-- | linear scanner
incrScanner :: forall st tok. Scanner st tok -> Scanner (st, [tok]) (Result tok)
incrScanner input = Scanner 
    {
      scanInit = (scanInit input, []),
      scanLooked = scanLooked input . fst,
      scanRun = run,
      scanEmpty = Result [] []
    }
    where
        run (st,partial) = updateState partial $ scanRun input st

        updateState _        [] = []
        updateState curState toks@((st,tok):rest) = ((st, curState), result) : updateState nextState rest
            where nextState = tok : curState
                  result    = Result curState (fmap snd toks)

getStrokes :: Point -> Point -> Point -> Result Stroke -> [Stroke]
getStrokes _point begin end (Result leftHL rightHL) = reverse (usefulsL leftHL) ++ usefulsR rightHL
    where
      usefulsR = dropWhile (\(_l,_s,r) -> r <= begin) .
                 takeWhile (\(l,_s,_r) -> l <= end)

      usefulsL = dropWhile (\(l,_s,_r) -> l >= end) .
                 takeWhile (\(_l,_s,r) -> r >= begin)


data ExtHL syntax = forall a. ExtHL (Highlighter a syntax) 




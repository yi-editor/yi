{-# LANGUAGE Rank2Types #-}
-- Copyright (C) 2008 JP Bernardy

module Yi.Syntax.Linear
  (tokAtOrBefore, getStrokes, incrScanner, Result) 
where

import Yi.Prelude
import Prelude ()
import Data.List (takeWhile, dropWhile, reverse)
import Yi.Syntax
import Yi.Region
import Yi.Lexer.Alex
import Data.Maybe (listToMaybe)

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

tokAtOrBefore :: Point -> Result (Tok a) -> Maybe (Tok a)
tokAtOrBefore p res = listToMaybe $ reverse $ toksInRegion (mkRegion 0 (p+1)) res

toksInRegion :: Region -> Result (Tok a) -> [Tok a]
toksInRegion reg (Result lefts rights) = reverse (usefulsL lefts) ++ usefulsR rights
    where
      usefulsR = dropWhile (\t -> tokEnd t   <= regionStart reg) .
                 takeWhile (\t -> tokBegin t <= regionEnd   reg)

      usefulsL = dropWhile (\t -> tokBegin t >= regionEnd   reg) .
                 takeWhile (\t -> tokEnd t   >= regionStart reg)


getStrokes :: Result Stroke -> Point -> Point -> Point -> [Stroke]
getStrokes (Result leftHL rightHL) _point begin end = reverse (usefulsL leftHL) ++ usefulsR rightHL
    where
      usefulsR = dropWhile (\(_l,_s,r) -> r <= begin) .
                 takeWhile (\(l,_s,_r) -> l <= end)

      usefulsL = dropWhile (\(l,_s,_r) -> l >= end) .
                 takeWhile (\(_l,_s,r) -> r >= begin)







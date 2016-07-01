{-# LANGUAGE ScopedTypeVariables #-}

module Yi.IncrementalParse (recoverWith, symbol, eof, lookNext, testNext,
                            State, P, Parser(..), AlexState (..), scanner) where

import           Parser.Incremental (Parser (..), Process, eof, evalL, evalR,
                                     lookNext, mkProcess, pushEof, pushSyms,
                                     recoverWith, symbol, testNext)

import           Yi.Lexer.Alex      (AlexState (..))
import           Yi.Syntax          (Scanner (..))

type P s a = Parser s a

type State st token result = (st, Process token result)

scanner :: forall st token result. Parser token result -> Scanner st token -> Scanner (State st token result) result
scanner parser input = Scanner
    {
      scanInit = (scanInit input, mkProcess parser),
      scanLooked = scanLooked input . fst,
      scanRun = run,
      scanEmpty = fst $ evalR $ pushEof $ mkProcess parser
    }
    where
        run :: State st token result -> [(State st token result, result)]
        run (st,process) = updateState0 process $ scanRun input st

        updateState0 :: Process token result -> [(st,token)] -> [(State st token result, result)]
        updateState0 _        [] = []
        updateState0 curState toks@((st,tok):rest) = ((st, curState), result) : updateState0 nextState rest
            where nextState =       evalL $           pushSyms [tok]           curState
                  result    = fst $ evalR $ pushEof $ pushSyms (fmap snd toks) curState

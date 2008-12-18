-- Copyright (c) JP Bernardy 2008
{-# OPTIONS -fglasgow-exts #-}
module Yi.IncrementalParse (recoverWith, symbol, eof, lookNext, testNext,
                            P, AlexState (..), scanner) where
import Parser.Incremental
import Yi.Lexer.Alex (AlexState (..))
import Yi.Prelude
import Prelude ()
import Yi.Syntax


type State st token result = (st, Process token result)

scanner :: forall st token result. P token result -> Scanner st token -> Scanner (State st token result) result
scanner parser input = Scanner 
    {
      scanInit = (scanInit input, runP parser),
      scanLooked = scanLooked input . fst,
      scanRun = run,
      scanEmpty = fst $ evalR $ pushEof $ runP parser
    }
    where
        run :: State st token result -> [(State st token result, result)]
        run (st,process) = updateState0 process $ scanRun input st

        updateState0 :: Process token result -> [(st,token)] -> [(State st token result, result)]
        updateState0 _        [] = []
        updateState0 curState toks@((st,tok):rest) = ((st, curState), result) : updateState0 nextState rest
            where nextState =       evalL $           pushSyms [tok]           $ curState
                  result    = fst $ evalR $ pushEof $ pushSyms (fmap snd toks) $ curState


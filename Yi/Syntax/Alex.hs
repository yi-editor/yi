{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex (mkHighlighter, 
                       alexGetChar, alexInputPrevChar,
                       AlexState, AlexInput, Stroke,
                       takeLB, actionConst, actionAndModify) where

import Data.List
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LB
import Yi.Syntax

takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take

type AlexInput  = LB.ByteString
type Action hlState token = AlexInput -> hlState -> (hlState, token)
type State hlState = (hlState, [Stroke])
type AlexState hlState = (Int, AlexInput, hlState)
type Result = [Stroke]
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | LB.null bs = Nothing
               | otherwise  = Just (LB.head bs, LB.tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

actionConst :: token -> Action lexState token
actionConst token _str state = (state, token)

actionAndModify :: (lexState -> lexState) -> token -> Action lexState token
actionAndModify modifier token _str state = (modifier state, token)


data Cache s = Cache  
    [(Int,State s)] -- list of cached intermediate states and their offsets.
    ([Stroke],[Stroke])

-- Unfold, scanl and foldr at the same time :)
-- origami :: (b -> Maybe (a, b)) -> b -> (a -> c -> c) -> (c -> c) -> c -> ([(b, c -> c)], c)
origami :: (b -> Maybe (a, b)) -> b -> (a -> c -> c) -> (c -> a -> c) 
        -> c -> c -> ([(b, c)], c)
origami gen seed (<+) (+>) l_c r_c = case gen seed of
      Nothing -> ([], r_c)
      Just (a, new_seed) -> 
          let ~(partials,c) = origami gen new_seed (<+) (+>) (l_c +> a) r_c
          in ((seed,l_c):partials,l_c `seq` a <+ c)

-- | Highlighter based on an Alex lexer 
mkHighlighter :: forall s. s
              -> (AlexState s -> Maybe (Stroke, AlexState s))
              -> Yi.Syntax.Highlighter (Cache s)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { hlStartState   = Cache [] ([],[])
                  , hlRun          = run
                  , hlGetStrokes   = getStrokes
                  }
      where 
        startState = (initState, [])
        getStrokes begin end (Cache _ (leftHL, rightHL)) = reverse (usefulsL leftHL) ++ usefulsR rightHL
            where
              usefulsR = dropWhile (\(_l,_s,r) -> r <= begin) .
                        takeWhile (\(l,_s,_r) -> l <= end)
                        
              usefulsL = dropWhile (\(l,_s,_r) -> l >= end) .
                         takeWhile (\(_l,_s,r) -> r >= begin)

        run getInput dirtyOffset (Cache cachedStates _) = Cache newCachedStates (leftResult,rightResult)
            where resumeIndex = fst resumeState
                  reused = takeWhile ((< dirtyOffset) . fst) cachedStates
                  resumeState = if null reused then (0, startState) else last reused
                  newCachedStates = reused ++ other 20 0 (drop 1 recomputed)
                  (recomputed, leftResult, rightResult) = updateState text resumeState
                  text = getInput resumeIndex


        updateState :: AlexInput -> (Int,State s) -> ([(Int,State s)], Result, Result)
        updateState input (startIdx, (restartState, startPartial)) = (map f partials, startPartial, result)
                where result :: [Stroke]
                      (partials,result) = origami alexScanToken (startIdx,input,restartState) (:) (flip (:)) startPartial []
                      f :: (AlexState s, [Stroke]) -> (Int,State s)
                      f ((idx,_input,state),partial) = (idx, (state,partial))


other :: Int -> Int -> [a] -> [a]
other n m l = case l of
                [] -> []
                (h:t) ->
                    case m of
                      0 -> h:other n n     t
                      _ ->   other n (m-1) t

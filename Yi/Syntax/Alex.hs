{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex (mkHighlighter, 
                       alexGetChar, alexInputPrevChar,
                       AlexState(..), AlexInput, Stroke,
                       takeLB, actionConst, actionAndModify) where

import Data.List hiding (map)
import qualified Data.ByteString.Lazy.Char8 as LB
import Yi.Syntax
import Yi.Prelude
import Prelude ()

takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take

type LookedOffset = Int -- ^ if offsets before this is dirtied, must restart from that state.
type AlexInput  = LB.ByteString
type Action hlState token = AlexInput -> hlState -> (hlState, token)
type State hlState = (AlexState hlState, [Stroke]) -- ^ Lexer state; (reversed) list of tokens so far.
data AlexState hlState = AlexState { 
      startOffset :: Int,       -- Start offset
      hlState :: hlState,   -- (user defined) lexer state
      lookedOffset :: LookedOffset, -- Last offset looked at
      curLine :: Int,
      curCol :: Int
    }

type Result = ([Stroke], [Stroke])
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | LB.null bs = Nothing
               | otherwise  = Just (LB.head bs, LB.tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

actionConst :: token -> Action lexState token
actionConst token _str state = (state, token)

actionAndModify :: (lexState -> lexState) -> token -> Action lexState token
actionAndModify modifier token _str state = (modifier state, token)

data Cache s = Cache [State s] Result

-- Unfold, scanl and foldr at the same time :)
origami :: (b -> Maybe (a, b)) -> b -> (a -> c -> c) -> (c -> a -> c) 
        -> c -> c -> ([(b, c)], c)
origami gen seed (<+) (+>) l_c r_c = case gen seed of
      Nothing -> ([], r_c)
      Just (a, new_seed) -> 
          let ~(partials,c) = origami gen new_seed (<+) (+>) (l_c +> a) r_c
          in ((seed,l_c):partials,l_c `seq` a <+ c)

type ASI s = (AlexState s, AlexInput)

-- | Highlighter based on an Alex lexer 
mkHighlighter :: forall s. s
              -> (ASI s -> Maybe (Stroke, ASI s))
              -> Yi.Syntax.Highlighter (Cache s)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { hlStartState   = Cache [] ([],[])
                  , hlRun          = run
                  , hlGetStrokes   = getStrokes
                  }
      where 
        startState = (AlexState 0 initState 0 1 0, [])
        getStrokes begin end (Cache _ (leftHL, rightHL)) = reverse (usefulsL leftHL) ++ usefulsR rightHL
            where
              usefulsR = dropWhile (\(_l,_s,r) -> r <= begin) .
                        takeWhile (\(l,_s,_r) -> l <= end)
                        
              usefulsL = dropWhile (\(l,_s,_r) -> l >= end) .
                         takeWhile (\(_l,_s,r) -> r >= begin)

        run getInput dirtyOffset (Cache cachedStates _) = -- trace (show $ map trd3 $ newCachedStates) $
            Cache newCachedStates result
            where resumeIndex = startOffset $ fst $ resumeState
                  reused = takeWhile ((< dirtyOffset) . lookedOffset . fst) cachedStates
                  resumeState = if null reused then startState else last reused
                  newCachedStates = reused ++ other 20 0 (drop 1 recomputed)
                  (recomputed, result) = updateState text resumeState
                  text = getInput resumeIndex


        updateState :: AlexInput -> State s -> ([State s], Result)
        updateState input (restartState, startPartial) = 
            (map f partials, (startPartial, result))
                where result :: [Stroke]
                      (partials,result) = origami alexScanToken (restartState, input) (:) (flip (:)) startPartial []
                      f :: ((AlexState s, AlexInput), [Stroke]) -> State s
                      f ((s, _), partial) = (s, partial)

other :: Int -> Int -> [a] -> [a]
other n m l = case l of
                [] -> []
                (h:t) ->
                    case m of
                      0 -> h:other n n     t
                      _ ->   other n (m-1) t

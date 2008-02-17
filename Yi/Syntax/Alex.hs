{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex where

import Data.List
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LB
import Yi.Syntax
import Yi.Style

takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take

type Length = Int

type AlexInput  = LB.ByteString
type Action hlState = AlexInput -> hlState -> (hlState, Style)
type State hlState = (hlState, [Stroke])
type AlexState hlState = (Int, AlexInput, hlState)
type Result = [Stroke]
type Endo a = a -> a
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | LB.null bs = Nothing
               | otherwise  = Just (LB.head bs, LB.tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

c :: Style -> Action a
c color _str state = (state, color)

m :: (s -> s) -> Style -> Action s
m modifier color _str state = (modifier state, color)

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
              -> Yi.Syntax.Highlighter (State s)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { Yi.Syntax.hlStartState   = (initState, [])
                  , Yi.Syntax.hlRun          = fun
                  }
      where fun :: AlexInput -> (Int,State s) -> ([(Int,State s)], Result, Result)
            fun input (startIdx, (startState, startPartial)) = (map f partials, startPartial, result)
                where result :: [Stroke]
                      (partials,result) = origami alexScanToken (startIdx,input,startState) (:) (flip (:)) startPartial []
                      f :: (AlexState s, [Stroke]) -> (Int,State s)
                      f ((idx,_input,state),partial) = (idx, (state,partial))

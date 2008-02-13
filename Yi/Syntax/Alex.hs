{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex where

import Data.List
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Yi.Syntax
import Yi.Style


takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take


type AlexInput  = LB.ByteString
type Action a   = AlexInput -> a -> (a, (Int, Style))
type State hlState = (hlState, Endo Result)
type AlexState hlState = (Int, AlexInput, hlState)
type Result = [(Int, Style)]
type Endo a = a -> a
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | LB.null bs = Nothing
               | otherwise  = Just (LB.head bs, LB.tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

c :: Style -> Action a
c color str state = (state, (fromIntegral $ LB.length str, color))

m :: (s -> s) -> Style -> Action s
m modifier color str state = (modifier state, (fromIntegral $ LB.length str, color))

-- Unfold, scanl and foldr at the same time :)
origami :: (b -> Maybe (a, b)) -> b -> (a -> c -> c) -> (c -> c) -> c -> ([(b, c -> c)], c)
origami gen seed (+>) l_c r_c = -- helper see
    
    case gen seed of
      Nothing -> ([], r_c)
      Just (a, new_seed) -> let ~(partials,c) = origami gen new_seed (+>) (l_c . (a +>)) r_c
                            in ((seed,l_c):partials,a +> c)


mkHighlighter :: forall s. s
              -> (AlexState s -> Maybe ((Int, Style), AlexState s))
              -> Yi.Syntax.Highlighter (State s)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { Yi.Syntax.hlStartState   = (initState, id)
                  , Yi.Syntax.hlRun          = fun
                  }
      where fun :: AlexInput -> (Int,State s) -> ([(Int,State s)], Result)
            fun input (startIdx, (startState, startPartial)) = (map f partials, startPartial result)
                where result :: Result
                      (partials,result) = origami alexScanToken (startIdx,input,startState) (:) startPartial [] 
                      f :: (AlexState s, Endo Result) -> (Int,State s)
                      f ((idx,_input,state),partial) = (idx, (state,partial))

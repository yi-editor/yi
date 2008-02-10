module Yi.Syntax.Alex where

import Data.List
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Yi.Syntax
import Yi.Style


takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take

{-# OPTIONS -fglasgow-exts #-}

type AlexInput  = LB.ByteString
type Action a   = AlexInput -> a -> (a, (Int, Style))
type State hlState = (hlState, Result)
type AlexState hlState = (AlexInput, hlState)
type Result = [(Int, Style)]

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | LB.null bs = Nothing
               | otherwise  = Just (LB.head bs, LB.tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

c :: Style -> Action a
c color str state = (state, (fromIntegral $ LB.length str, color))

m :: (s -> s) -> Style -> Action s
m modifier color str state = (modifier state, (fromIntegral $ LB.length str, color))

mkHighlighter :: s
              -> ((LB.ByteString, s) -> Maybe ((s, (Int, Style)), (LB.ByteString, s)))
              -> Yi.Syntax.Highlighter (State s)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { Yi.Syntax.hlStartState   = (initState, [])
                  , Yi.Syntax.hlRun          = fun
                  , Yi.Syntax.hlGetResult    = snd
                  }
      where --fun :: AlexInput -> (Int, State) -> [(Int, State)]
            fun input (startIndex, (state, resultSoFar)) = scanl f (startIndex, (state, resultSoFar)) tokens
                where tokens = unfoldr alexScanToken (input,state)
                      --f :: (Int, State) -> (HlState, (Int, Style)) -> (Int,State)
                      f (idx, (_s, r)) (s',r') = (idx + fst r', (s', r ++ [r']))

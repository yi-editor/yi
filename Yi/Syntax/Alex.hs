{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex (
                       mkHighlighter,
                       alexGetChar, alexInputPrevChar, unfoldLexer, lexScanner,
                       AlexState(..), AlexInput, Stroke,
                       takeLB, headLB, actionConst, actionAndModify,
                       Tok(..), tokBegin, tokEnd, tokFromT,          
                       Posn(..), startPosn, moveStr, runSource,
                       Result,
                      ) where

import Data.List hiding (map)
import qualified Data.ByteString.Lazy.Char8 as LB
import Yi.Syntax
import Yi.Prelude
import Prelude ()

takeLB :: Int64 -> LB.ByteString -> LB.ByteString
takeLB = LB.take

headLB :: LB.ByteString -> Char
headLB = LB.head


-- | if offsets before this is dirtied, must restart from that state.
type LookedOffset = Int

type AlexInput = [(Int, Char)]
type Action hlState token = AlexInput -> hlState -> (hlState, token)
-- | Lexer state; (reversed) list of tokens so far.
type State hlState = (AlexState hlState, [Stroke])
data AlexState lexerState = AlexState {
      stLexer  :: lexerState,   -- (user defined) lexer state
      lookedOffset :: !LookedOffset, -- Last offset looked at
      stPosn :: !Posn
    } deriving Show



data Tok t = Tok
    {
     tokT :: t,
     tokLen  :: Int,
     tokPosn :: Posn
    }

tokFromT :: forall t. t -> Tok t
tokFromT t = Tok t 0 startPosn

tokBegin :: forall t. Tok t -> Int
tokBegin = posnOfs . tokPosn

tokEnd :: forall t. Tok t -> Int
tokEnd t = tokBegin t + tokLen t

instance Show t => Show (Tok t) where
    show tok = show (tokPosn tok) ++ ": " ++ show (tokT tok)

type Result = ([Stroke], [Stroke])

data Posn = Posn {posnOfs :: !Int, posnLine :: !Int, posnCol :: !Int}

instance Show Posn where
    show (Posn _ l c) = "L" ++ show l ++ " " ++ "C" ++ show c

startPosn :: Posn
startPosn = Posn 0 1 0

moveStr :: Posn -> AlexInput -> Posn
moveStr posn str = foldl' moveCh posn (fmap snd str)

moveCh :: Posn -> Char -> Posn
moveCh (Posn o l c) '\t' = Posn (o+1)  l     (((c+8) `div` 8)*8)
moveCh (Posn o l _) '\n' = Posn (o+1) (l+1)   0
moveCh (Posn o l c) _    = Posn (o+1) l     (c+1)


alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | null bs = Nothing
               | otherwise  = Just (snd $ head bs, tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar undefined"

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

-- FIXME: this should take a generic Source and + Token -> Stroke function. 
-- | Highlighter based on an Alex lexer 
mkHighlighter :: forall s. s
              -> (ASI s -> Maybe (Stroke, ASI s))
              -> Yi.Syntax.Highlighter' (Cache s) Result
mkHighlighter initState alexScanToken =
  Yi.Syntax.SynHL { hlStartState   = Cache [] ([],[])
                  , hlRun          = run
                  , hlGetStrokes   = getStrokes
                  , hlGetTree      = \(Cache _ result) -> result
                  }
      where
        startState = (AlexState initState 0 startPosn, [])
        getStrokes point begin end (Cache _ (leftHL, rightHL)) = reverse (usefulsL leftHL) ++ usefulsR rightHL
            where
              usefulsR = dropWhile (\(_l,_s,r) -> r <= begin) .
                         takeWhile (\(l,_s,_r) -> l <= end)

              usefulsL = dropWhile (\(l,_s,_r) -> l >= end) .
                         takeWhile (\(_l,_s,r) -> r >= begin)

        run scanner dirtyOffset (Cache cachedStates _) = -- trace (show $ map trd3 $ newCachedStates) $
            Cache newCachedStates result
            where resumeIndex = posnOfs $ stPosn $ fst $ resumeState
                  reused = takeWhile ((< dirtyOffset) . lookedOffset . fst) cachedStates
                  resumeState = if null reused then startState else last reused
                  newCachedStates = reused ++ other 20 0 (drop 1 recomputed)
                  (recomputed, result) = updateState text resumeState
                  text = scanRun scanner resumeIndex


        updateState :: AlexInput -> State s -> ([State s], Result)
        updateState input (restartState, startPartial) =
            (fmap f partials, (startPartial, result))
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


runSource :: forall t t1. Scanner t t1 -> [(t, t1)]
runSource (Scanner initSt f) = f initSt

lexScanner :: forall lexerState token a.
                                          ((AlexState lexerState, [(Int, a)])
                                           -> Maybe (token, (AlexState lexerState, [(Int, a)])))
                                          -> lexerState
                                          -> Scanner Int a
                                          -> Scanner (AlexState lexerState) token
lexScanner l st0 src = Scanner
                 {
                  --stStart = posnOfs . stPosn,
                  --stLooked = lookedOffset,
                  scanInit = AlexState st0 0 startPosn,
                  scanRun = \st -> unfoldLexer l (st, scanRun src $ posnOfs $ stPosn st)
                 }

-- | unfold lexer function into a function that returns a stream of (state x token)
unfoldLexer :: ((AlexState lexState, input) -> Maybe (token, (AlexState lexState, input)))
             -> (AlexState lexState, input) -> [(AlexState lexState, token)]
unfoldLexer f b = case f b of
             Nothing -> []
             Just (t, b') -> (fst b, t) : unfoldLexer f b'

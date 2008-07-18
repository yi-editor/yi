{-# OPTIONS -fglasgow-exts #-}

module Yi.Syntax.Alex (
                       alexGetChar, alexInputPrevChar, unfoldLexer, lexScanner,
                       AlexState(..), AlexInput, Stroke,
                       actionConst, actionAndModify,
                       Tok(..), tokBegin, tokEnd, tokFromT, tokRegion, 
                       Posn(..), startPosn, moveStr, 
                       ASI,
                       (+~), (~-), Size(..)
                      ) where

import Yi.Syntax hiding (mkHighlighter)
import Yi.Prelude
import Prelude ()
import Data.Char (ord)
import Yi.Region

-- | if offsets before this is dirtied, must restart from that state.
type LookedOffset = Point

type AlexInput = [(Point, Char)]
type Action hlState token = AlexInput -> hlState -> (hlState, token)
-- | Lexer state; (reversed) list of tokens so far.
data AlexState lexerState = AlexState {
      stLexer  :: lexerState,   -- (user defined) lexer state
      lookedOffset :: !LookedOffset, -- Last offset looked at
      stPosn :: !Posn
    } deriving Show



data Tok t = Tok
    {
     tokT :: t,
     tokLen  :: Size,
     tokPosn :: Posn
    }

tokFromT :: forall t. t -> Tok t
tokFromT t = Tok t 0 startPosn

tokBegin :: forall t. Tok t -> Point
tokBegin = posnOfs . tokPosn

tokEnd :: forall t. Tok t -> Point
tokEnd t = tokBegin t +~ tokLen t

tokRegion :: Tok t -> Region
tokRegion t = mkRegion (tokBegin t) (tokEnd t)


instance Show t => Show (Tok t) where
    show tok = show (tokPosn tok) ++ ": " ++ show (tokT tok)

data Posn = Posn {posnOfs :: !Point, posnLine :: !Int, posnCol :: !Int}

instance Show Posn where
    show (Posn _ l c) = "L" ++ show l ++ " " ++ "C" ++ show c

startPosn :: Posn
startPosn = Posn 0 1 0

moveStr :: Posn -> AlexInput -> Posn
moveStr posn str = foldl' moveCh posn (fmap snd str)

utf8Length :: Char -> Size
utf8Length c = let i = ord c in
                 if i < 0x80 then
                     1
                 else if i < 0x800 then
                     2
                 else if i < 0x10000 then
                     3
                 else 
                     4

moveCh :: Posn -> Char -> Posn
moveCh (Posn o l c) '\t' = Posn (o+1)  l     (((c+8) `div` 8)*8)
moveCh (Posn o l _) '\n' = Posn (o+1) (l+1)   0
moveCh (Posn o l c) chr  = Posn (o+~utf8Length chr) l     (c+1)


alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar bs | null bs = Nothing
               | otherwise  = Just (snd $ head bs, tail bs)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar undefined"

actionConst :: token -> Action lexState token
actionConst token _str state = (state, token)

actionAndModify :: (lexState -> lexState) -> token -> Action lexState token
actionAndModify modifier token _str state = (modifier state, token)


type ASI s = (AlexState s, AlexInput)

lexScanner :: forall lexerState token.
                                          ((AlexState lexerState, [(Point, Char)])
                                           -> Maybe (token, (AlexState lexerState, [(Point, Char)])))
                                          -> lexerState
                                          -> Scanner Point Char
                                          -> Scanner (AlexState lexerState) token
lexScanner l st0 src = Scanner
                 {
                  --stStart = posnOfs . stPosn,
                  scanLooked = lookedOffset,
                  scanInit = AlexState st0 0 startPosn,
                  scanRun = \st -> unfoldLexer l (st, scanRun src $ posnOfs $ stPosn st)
                 }

-- | unfold lexer function into a function that returns a stream of (state x token)
unfoldLexer :: ((AlexState lexState, input) -> Maybe (token, (AlexState lexState, input)))
             -> (AlexState lexState, input) -> [(AlexState lexState, token)]
unfoldLexer f b = case f b of
             Nothing -> []
             Just (t, b') -> (fst b, t) : unfoldLexer f b'

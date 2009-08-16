module Yi.Syntax.Strokes.Haskell (getStrokes, tokenToAnnot) where

import Prelude ()
import Data.Maybe
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Yi.Style
import Yi.Syntax
import Yi.Prelude
import Prelude ()
import Data.Monoid
import Data.Maybe
import Yi.Syntax.Haskell
import Yi.Syntax.Tree (subtrees)

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes point begin _end t0 = trace (show t0) result
    where result = appEndo (getStr tkDConst point begin _end t0) []

-- | Get strokes Module for module
getStrokeMod :: Point -> Point -> Point -> PModuleDecl TT -> Endo [Stroke]
getStrokeMod point begin _end tm@(PModuleDecl m na e w)
    = pKW tm m <> getStr tkImport point begin _end na
   <> getStrokes' e <> getStrokes' w
        where getStrokes' r = getStr tkDConst point begin _end r
              pKW b word | isErrN b = paintAtom errorStyle word
                         | otherwise = getStrokes' word

-- | Get strokes for Imports
getStrokeImp ::  Point -> Point -> Point -> PImport TT -> Endo [Stroke]
getStrokeImp point begin _end imp@(PImport m qu na t t')
    = pKW imp m <> paintQu qu
   <> getStr tkImport point begin _end na <> paintAs t  <> paintHi t'
    where getStrokes' r = getStr tkDConst point begin _end r
          paintAs (Opt (Just (Bin (PAtom n c) tw)))
              = (one $ (fmap (const keywordStyle) . tokToSpan) n) <> com c
             <> getStr tkImport point begin _end tw
          paintAs a = getStrokes' a
          paintQu (Opt (Just ((PAtom n c)))) = (one $ (fmap (const keywordStyle) . tokToSpan) n) <> com c
          paintQu a = getStrokes' a
          paintHi (Bin (Bin (PAtom n c) tw) r) = (one $ (fmap (const keywordStyle) . tokToSpan) n)
                                             <> com c <> getStr tkImport point begin _end tw
                                             <> getStrokes' r
          paintHi a = getStrokes' a
          pKW b word | isErrN b = paintAtom errorStyle word
                     | otherwise = getStrokes' word

-- | Get strokes for expressions and declarations
getStr :: (TT -> Endo [Stroke]) -> Point -> Point -> Point -> Exp TT 
          -> Endo [Stroke]
getStr tk point begin _end t0 = getStrokes' t0
    where getStrokes' :: Exp TT -> Endo [Stroke]
          getStrokes' t@(PImport {}) = getStrokeImp point begin _end t
          getStrokes' t@(PModuleDecl {}) = getStrokeMod point begin _end t
          getStrokes' (PModule c m) = com c <> foldMap getStrokes' m
          getStrokes' (PAtom t c) = tk t <> com c
          getStrokes' (TS col ts') = tk col <> foldMap (getStr tkTConst point begin _end) ts'
          getStrokes' (Modid t c) = tkImport t <> com c
          getStrokes' (Paren (PAtom l c) g (PAtom r c'))
              | isErr r = errStyle l <> getStrokesL g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) ==
                    point || (posnOfs $ tokPosn $ r) == point - 1
               = pStyle hintStyle l <> com c <> getStrokesL g
                      <> pStyle hintStyle r <> com c'
              | otherwise  = tk l <> com c <> getStrokesL g
                                  <> tk r <> com c'
          getStrokes' (PError t _ c) = errStyle t <> com c
          getStrokes' (Block s) = getStrokesL s
          getStrokes' (Expr g) = getStrokesL g
          getStrokes' (PWhere e exp c) = getStrokes' e <> getStrokes' exp <> getStrokes' c
          getStrokes' (RHS eq g) = getStrokes' eq <> getStrokes' g
          getStrokes' (Bin l r) = getStrokes' l <> getStrokes' r
          getStrokes' ty@(PType e na eq b)
               = getStrokes' e <> getStrokes' na
              <> getStrokes' eq
              <> getStrokes' b
          getStrokes' da@(PData kw na exp eq)
               = pKW da kw <> getStrokes' na
              <> getStrokes' exp <> getStrokes' eq
          getStrokes' (PData' eq b) =
                getStrokes' eq <> getStrokes' b
          getStrokes' (PLet l expr i) =
                getStrokes' l <> getStrokes' expr <> getStrokes' i
          getStrokes' (PIn t l) = tk t <> getStrokesL l
          getStrokes' (Opt (Just l)) =  getStrokes' l
          getStrokes' (Opt Nothing) = getStrokesL []
          getStrokes' (Context fAll l arr) =
                getStrokes' fAll <> getStrokes' l <> getStrokes' arr
          getStrokes' (TC l) = getStr tkTConst point begin _end l
          getStrokes' (DC (PAtom l c)) = tkDConst l <> com c
          getStrokes' (DC r) = getStrokes' r -- do not color operator dc
          getStrokes' (PGuard ls) = getStrokesL ls
          getStrokes' g@(PGuard' t e t')
              = pKW g t <> getStrokes' e <> getStrokes' t'
          getStrokes' cl@(PClass e e' exp)
              = pKW cl e <> getStrokes' e'
             <> getStrokes' exp 
          getStrokes' t = foldMap getStrokes' (subtrees t) -- by default deal with subtrees
          getStrokesL = foldMap getStrokes'
          pKW b word | isErrN b = paintAtom errorStyle word
                     | otherwise = getStrokes' word

-- Stroke helpers follows

tokenToAnnot :: TT -> Maybe (Span String)
tokenToAnnot = sequenceA . tokToSpan . fmap tokenToText

ts :: TT -> Stroke
ts = tokenToStroke

pStyle :: StyleName -> TT -> Endo [Stroke]
pStyle style = one . (modStroke style) . ts

one :: Stroke -> Endo [Stroke]
one x = Endo (x :)

paintAtom :: StyleName -> (Exp TT) -> Endo [Stroke]
paintAtom col (PAtom a c) = pStyle col a <> com c
paintAtom _ _ = error "wrong usage of paintAtom"

isErr :: TT -> Bool
isErr = isErrorTok . tokT

isErrN :: (Foldable v) => (v TT) -> Bool
isErrN t = (any isErr t) 
--         || (not $ null $ isError' t)

errStyle :: TT -> Endo [Stroke]
errStyle = pStyle errorStyle

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f `mappend`)

com :: [TT] -> Endo [Stroke]
com r = foldMap tkDConst r

tk' :: (TT -> Bool) -> (TT -> Endo [Stroke]) -> TT -> Endo [Stroke]
tk' f s t | isErr t = errStyle t
          | elem (tokT t) (fmap Reserved [As, Qualified, Hiding]) 
            = one $ (fmap (const variableStyle) . tokToSpan) t
          | f t = s t
          | otherwise = one (ts t)

tkTConst :: TT -> Endo [Stroke]
tkTConst = tk' (const False) (const (Endo id))


tkDConst :: TT -> Endo [Stroke]
tkDConst = tk' ((== ConsIdent) . tokT) (pStyle dataConstructorStyle)

tkImport :: TT -> Endo [Stroke]
tkImport = tk' ((== ConsIdent) . tokT) (pStyle importStyle)
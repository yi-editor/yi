{-# LANGUAGE FlexibleInstances, TypeFamilies, TemplateHaskell #-}
-- Copyright (c) JP Bernardy 2008
-- | Parser for haskell that takes in account only parenthesis and layout
module Yi.Syntax.Haskell where

import Prelude ()
import Data.Maybe
import Data.List (filter, takeWhile)

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Yi.Style (hintStyle, errorStyle, StyleName)
import Yi.Syntax.Layout
import Yi.Syntax.Tree
import qualified Yi.Syntax.BList as BL
import Yi.Syntax
import Yi.Prelude
import Prelude ()
import Data.Monoid
import Data.DeriveTH
import Data.Derive.Foldable
import Data.Maybe

indentScanner :: Scanner (AlexState lexState) (TT)
              -> Scanner (Yi.Syntax.Layout.State Token lexState) (TT)
indentScanner = layoutHandler startsLayout [(Special '(', Special ')'),
                                            (Special '[', Special ']'),
                                            (Special '{', Special '}')] ignoredToken
                         (fmap Special ['<', '>', '.']) isBrace

-- HACK: We insert the Special '<', '>', '.', that don't occur in normal haskell
-- parsing.

isBrace :: TT -> Bool
isBrace (Tok b _ _) = (Special '{') == b

ignoredToken :: TT -> Bool
ignoredToken (Tok t _ (Posn _ _ _)) = isComment t || t == CppDirective

-- isNoise :: Token -> Bool
isNoise :: (Char -> Bool) -> Token -> Bool
-- isNoise (Special _) = False
isNoise f (Special c) =  f c
-- isNoise _ (ReservedOp _) = False
-- isNoise _ (ReservedOp RightArrow) = False
-- isNoise _ (ReservedOp Equal) = False
-- isNoise _ (VarIdent) = False
-- isNoise _ (Reserved _) = False
-- isNoise _ (CppDirective) = False
isNoise _ (Reserved Module) = False
isNoise _ (Reserved Import) = False
-- isNoise _ (Reserved Where) = False
-- isNoise _ (Reserved As) = False
isNoise _ (Reserved Type) = False
isNoise _ (Reserved Data) = False
--isNoise _ (Reserved NewType) = False
isNoise _ (Reserved Qualified) = False
isNoise _ (Reserved Hiding) = False
-- isNoise _ (Comment _) = False
-- isNoise _ (Operator _) = False
isNoise _ _ = True

data Tree t
    = Paren t (Tree t) t -- A parenthesized expression
    | Paren' t [t] (Tree t) t [t] -- an extended version of parenthesized expressions that can have following comments
    | Block (BL.BList [Tree t])      -- A list of things separated by layout (as in do; etc.)
    | Atom t
    | Expr [Tree t]
    | KW t (Tree t)
    | KW2 t [t] (Tree t)
    | Bin (Tree t) (Tree t)
    | Error t
    | Opt (Maybe (Tree t))
    | Comm (Tree t) [t]
    | Modid t
    | Mod t [t] t [t] (Tree t) t [t] (Tree t)
    | Imp t [t] (Tree t) t [t] (Tree t) (Tree t)
    | Typ t [t] (Tree t) (Tree t) (Tree t) (Tree t)
    | Dat t [t] (Tree t) (Tree t) (Tree t)
    | Dat' t [t] (Tree t) (Tree t)
    | TypeSig (Tree t) [Tree t]
  deriving Show

$(derive makeFoldable ''Tree)
instance IsTree Tree where
--    subtrees (Paren _ g _) = g
     subtrees (Block s)     = concat s
     subtrees (Expr a)      = a
     subtrees (TypeSig _ a) = a
     subtrees _             = []

-- | Search the given list, and return the 1st tree after the given
-- point on the given line.  This is the tree that will be moved if
-- something is inserted at the point.  Precondition: point is in the
-- given line.

-- TODO: this should be optimized by just giving the point of the end
-- of the line
getIndentingSubtree :: [Tree TT] -> Point -> Int -> Maybe (Tree TT)
getIndentingSubtree roots offset line =
    listToMaybe $ [t | (t,posn) <- takeWhile ((<= line) . posnLine . snd) $ allSubTreesPosn,
                   -- it's very important that we do a linear search
                   -- here (takeWhile), so that the tree is evaluated
                   -- lazily and therefore parsing it can be lazy.
                   posnOfs posn > offset, posnLine posn == line]
    where allSubTreesPosn = [(t',posn) | root <- roots, t'@(Block _) <-filter (not . null . toList) (getAllSubTrees root),
                             let (tok:_) = toList t',
                             let posn = tokPosn tok]

-- | given a tree, return (first offset, number of lines).
getSubtreeSpan :: Tree TT -> (Point, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last] = fmap (tokPosn . assertJust) [getFirstElement tree, getLastElement tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x
          assertJust _ = error "assertJust: Just expected"

-- | The parser
parse :: P TT (Tree TT)
parse = parse' tokT tokFromT

parse' :: (TT -> Token) -> (Token -> TT) -> P TT (Tree TT)
parse' toTok fromT = pDecl <* eof

-- | Parse Variables
pVarId :: Parser TT TT
pVarId = sym $ exact' [VarIdent, (Reserved Other), (Reserved As)]

-- | Parse constructors
pConId :: Parser TT TT
pConId = exact ConsIdent

-- | Parse modules
pModid :: Parser TT (Tree TT)
pModid = Modid <$> pConId

pQvarid :: Parser TT (Tree TT) 
pQvarid = pCom' $ Atom <$> (sym $ exact' [VarIdent, ConsIdent, (Reserved Other), (Reserved As)])

ppQvarsym :: Parser TT (Tree TT)
ppQvarsym = pCom $ Atom <$> sym isOperator

pQtycon :: Parser TT (Tree TT)
pQtycon = pCom' $ Atom <$> pConId

pVars :: Parser TT (Tree TT)
pVars = pMany $ pCom' $ Atom <$> pVarId

pConop ::Parser TT (Tree TT)
pConop = pCom' $ Atom <$> sym isOperator

pQvar :: Parser TT (Tree TT)
pQvar = pQvarid <|> pTup (ppQvarsym)

-- | parse a special symbol
sym :: (Token -> Bool) ->Parser TT TT
sym f   = symbol (f . tokT)

exact :: Token -> Parser TT TT
exact s = sym (== s)

exact' :: [Token] -> (Token ->Bool)
exact' s = \x -> elem x s

spec :: Char -> Parser TT TT
spec '|' = exact (ReservedOp Pipe)
spec '=' = exact (ReservedOp Equal)
spec c   = sym (isSpecial [c])

-- | Create a special character symbol
newT :: Char -> TT
newT c = tokFromT (Special c)

pleaseSym :: Char -> Parser TT TT
pleaseSym c = (recoverWith (pure $ newT '!')) <|> spec c

pleaseB :: Parser TT TT -> Parser TT TT
pleaseB b   = (recoverWith (pure $ newT '!')) <|> b

pleaseC ::Parser TT (Tree TT) ->Parser TT (Tree TT)
pleaseC  c  = (Error <$> recoverWith (pure $ newT '!')) <|> c

ppCons :: Parser TT (Tree TT)
ppCons = pCom $ Atom <$> exact ConsIdent

pAs :: Parser TT (Tree TT)
pAs = Atom <$> exact (Reserved As)

pRArrow :: Parser TT (Tree TT)
pRArrow = Atom <$> exact (ReservedOp RightArrow)

ppCom ::Parser TT [TT]
ppCom = many $ sym (\x -> isComment x || (CppDirective == x))

pModule :: Parser TT (Tree TT)
pModule = (b (Reserved Module) ConsIdent ((optional (spec '.')) *> pleaseB (exact (Reserved Where))) pRest)
    where b m na w r
              = Mod <$> exact m             <*> ppCom
                    <*> pleaseB (exact na)  <*> ppCom
                    <*> pExports
                    <*> w                   <*> ppCom
                    <*> r
          pExports = pOpt $ pTup $ Bin <$> pMany (Bin <$> pExport <*> pComma) <*> pOpt pExport -- optional trailing comma
          pExport = (optional $ spec '.') *> 
                      (pQvarid
                      <|> (pCom' pEModule)
                      <|> Bin <$> pTup (ppQvarsym) <*> pOpt helper
                      <|> (Bin <$> (pQtycon)
                            <*> pleaseC helper)
                      <|> (Error <$> recoverWith (pure $ newT '!')))
          helper = pTup ((pCom' $ Atom <$> exact (ReservedOp (OtherOp "..")))
                               <|> (Bin <$> pSome (Bin <$> 
                                                   (pQvarid
                                                   <|> pTup (ppQvarsym)) <*> pleaseC pComma)
                                    <*> pOpt pQvar) -- optional trailing comma
                               <|> (Error <$> recoverWith (pure $ newT '!')))
          pRest = (pBlockOf' (spec '.' *> pImp <|> pt)) 
                   <|> (spec '.' *> pt) 
                   <|> Expr <$> pure []
                   <|> (Error <$> recoverWith (symbol $ const True)) -- catch all errors

pOpt :: Parser TT (Tree TT) -> Parser TT (Tree TT)
pOpt p   = Opt  <$> optional p

pCom :: Parser TT (Tree TT) -> Parser TT (Tree TT)
pCom b   = Comm <$> pleaseC b <*> ppCom

pCom' :: Parser TT (Tree TT) -> Parser TT (Tree TT)
pCom' b  = Comm <$> b         <*> ppCom

pComment :: Parser TT TT
pComment = sym isComment

-- | Parse tuple, paren
pTup :: Parser TT (Tree TT) -> Parser TT (Tree TT)
pTup p = (Paren'  <$>  spec '(' <*> ppCom
          <*> p <*> pleaseSym ')' <*> ppCom)

pImp :: Parser TT (Tree TT)
pImp = Bin <$> pImp' <*> ((spec '.' *> (pImp <|> pt)) <|> (Expr <$> pure []) <|> (Error <$> recoverWith (symbol $ const True)))

-- | Parse imports
pImp' :: Parser TT (Tree TT)
pImp' = pI (exact (Reserved Import)) (pOpt (pCom' $ Atom <$> exact (Reserved Qualified))) (pleaseB $ exact ConsIdent)
    where pI na qu pId
              = Imp  <$> na       <*> ppCom
                     <*> qu
                     <*> pId      <*> ppCom
                     <*> pOpt pAs'
                     <*> pImpSpec
          pAs'     = Bin  <$> pCom' pAs <*> ppCons
          pImpSpec = pOpt (Bin <$> (pOpt (pCom' (Atom <$> exact (Reserved Hiding)))) <*> pleaseC pImpS)
          pImpS    = pTup $ Bin <$> (pMany (Bin <$> pExp' <*>  pComma)) <*> pOpt pExp' -- trailing comma is optional
          pExp'    = Bin <$> (pCom' (Atom <$> sym (\x -> (exact' [VarIdent, ConsIdent] x) || isOperator x))
                              <|> pTup ppQvarsym) <*> pOpt pImpS

-- | Parse simple types
pSType :: Parser TT (Tree TT)
pSType = Typ <$> exact (Reserved Type)     <*> ppCom
             <*> ppCons                    <*> pMany pQvarid-- (Atom <$> (sym $ exact' [ConsIdent, VarIdent]))
             <*> pCom (Atom <$> spec '=')
             <*> pType <* ((Atom <$> spec '.') <|> (Error <$> recoverWith (symbol $ const True))) 

pType :: Parser TT (Tree TT)
-- pType = (Bin <$> pBtype <*> pOpt (Bin <$> pCom' pRArrow <*> pleaseC pType)) -- pMany (Bin <$> (pCom' pRArrow) <*> pleaseC pBtype))
pType = Block <$> some pAtype `BL.sepBy1` (pCom' pRArrow)

pSData :: Parser TT (Tree TT)
pSData = Dat <$> exact (Reserved Data)    <*> ppCom
             <*> pOpt pContext
             <*> pSimpleType
             <*> pOpt pSData' <* pleaseSym '.'

pSData' :: Parser TT (Tree TT)
pSData' = (Dat' <$> (sym $ exact' [(ReservedOp Equal),(Reserved Where)]) <*> ppCom -- either we have standard data, or we have GADT:s
             <*> (pConstrs <|> (spec '<' *> (Block <$> many pGadt `BL.sepBy1` spec '.') <* spec '>'))
             <*> pOpt pDeriving) <|> pDeriving

pGadt :: Parser TT (Tree TT)
pGadt = Bin <$> pQtycon <*> pOpt (Bin <$> pCom (Atom <$> exact (ReservedOp (OtherOp "::")))
                                  <*> -- (Bin <$> pOpt (Bin <$> pContext <*> (Atom <$> exact (ReservedOp DoubleRightArrow)))
                                      (pType <|> (KW <$> exact (Operator "!") <*> pAtype) <|> (Error <$> recoverWith (symbol $ const True))))

pDeriving :: Parser TT (Tree TT)
pDeriving = Bin <$> pCom' (Atom <$> exact (Reserved Deriving)) 
                <*> pleaseC ((pTup ((Bin <$> pleaseC pQtycon <*> pMany (Bin <$> pComma <*> pQtycon))))
                             <|> pQtycon)

pAtype :: Parser TT (Tree TT)
pAtype = pQvarid -- (Atom <$> (sym $ exact' [VarIdent, ConsIdent]))
         <|> (pTup $ pMany pTree) -- ((Bin <$> pType <*> pMany (Bin <$> pComma <*> pType)) <|> (pCom' pRArrow)))
         <|> (pBrack' $ pMany pTree) -- pleaseC pAtype)
         <|> Atom <$> exact (ReservedOp DoubleRightArrow)
--          <|> (Error <$> recoverWith (pure $ newT '!'))
         <|> (Error <$> recoverWith (symbol $ const True))

pBtype :: Parser TT (Tree TT)
pBtype = pSome pAtype

pSimpleType :: Parser TT (Tree TT)
pSimpleType = Bin <$> ppCons <*> pMany pQvarid -- (Atom <$> sym (exact' [ConsIdent, VarIdent])))

pContext :: Parser TT (Tree TT)
pContext = pClass <|> pTup (pMany pClass)

-- add error
pClass :: Parser TT (Tree TT)
pClass = (Bin <$> pQtycon <*> ((pCom (Atom <$> pVarId)) <|> pTup (Bin <$> pCom (Atom <$> pVarId) <*> pSome (pCom pAtype)))) 

-- unsure if pAll and pCon is allowed together and in which order
pConstrs :: Parser TT (Tree TT)
pConstrs = Bin <$> (Bin <$> pOpt pAll <*> pConstr) <*> pMany (Bin <$> (Atom <$> spec '|') <*> (Bin <$> pOpt pAll <*> pConstr))
  where pAll = KW <$> exact (Reserved Forall) <*> (Bin <$> (Atom <$> pVarId) <*> (Atom <$> exact (Operator ".")))
        pCon = Bin <$> pContext   <*> (Atom <$> exact (ReservedOp DoubleRightArrow))
        allCon = Bin <$> pOpt pAll <*> pOpt pCon
--         pCon = Block <$> some (pConstr) `BL.sepBy1` spec '|'

pConstr :: Parser TT (Tree TT)
pConstr =  (Bin <$> pSome pAtype <*> pOpt st)
           <|> Bin <$> lrHs <*> pMany (strictF pAtype) -- (Bin <$> lrHs <*> sy)
           <|> (Error <$> recoverWith (symbol $ const True))
    where lrHs = ((KW <$> exact (Operator "!") <*> pAtype))
          st = ((pSome $ lrHs) <|> (pBrace' $ pOpt (Bin <$> pFielddecl <*> pMany (Bin <$> pComma <*> pFielddecl))))
--           sy = (Bin <$> pConop <*> lrHs)
--           pCon = pQtycon <|> pTup pConop

strictF :: Parser TT (Tree TT) -> Parser TT (Tree TT)
strictF a = Bin <$> pOpt (pCom' (Atom <$> exact (Operator "!"))) <*> a

pFielddecl ::Parser TT (Tree TT)
pFielddecl = (Bin <$> pVars <*> pOpt (Bin <$> pCom (Atom <$> exact (ReservedOp (OtherOp "::")))
                                         <*> (pType <|> (KW <$> exact (Operator "!") <*> pAtype)  <|> (Error <$> recoverWith (symbol $ const True)))))

pComma ::Parser TT (Tree TT)
pComma = (Comm <$> (Atom <$> spec ',') <*> ppCom)

isOperator ::Token -> Bool
isOperator (Operator _)     = True
isOperator (ReservedOp _)   = True
isOperator (ConsOperator _) = True
isOperator _                = False

-- | Exporting module
pEModule ::Parser TT (Tree TT)
pEModule = KW <$> exact (Reserved Module) <*> pleaseC pModid

pDecl :: Parser TT (Tree TT)
pDecl = Bin <$> (pMany (Atom <$> sym (exact' [(Comment Open), (Comment Close), (Comment Text), (CppDirective), (Comment Line)])))
            <*> pOpt (pBlockOf' (spec '.'
                           *> (pModule
                               <|> pImp
                               <|> pt)))

pt  = Block <$> (pBlocks pDTree)

pMany ::Parser TT (Tree TT) ->Parser TT (Tree TT)
pMany r = Expr <$> many r

pSome ::Parser TT (Tree TT) ->Parser TT (Tree TT)
pSome r = Expr <$> some r

pDTree :: Parser TT (Tree TT)
pDTree = (pBlockOf pDTree)
           <|> (Bin <$> pTree <*> pOpt (pBrace $ pMany pDTree)) -- small hack to improve

pBlocks p   = (many p) `BL.sepBy1` spec '.' -- see HACK above
pBlockOf p  = (Block <$> (spec '<' *> pBlocks p <* spec '>')) -- see HACK above
pBlockOf' p = ((spec '<' *> p <* spec '>')) -- see HACK above
-- also, we discard the empty statements

pTuple p  = (Paren  <$>  (spec '(') <*> p  <*> (pleaseSym ')'))
pBrack p  = (Paren  <$>  (spec '[') <*> p  <*> (pleaseSym ']'))
pBrace p  = (Paren  <$>  (spec '{') <*> p  <*> (pleaseSym '}'))
pBrace' p  = (Paren'  <$>  (spec '{') <*> ppCom <*> p  <*> (pleaseSym '}') <*> ppCom)
pBrack' p = (Paren'  <$>  spec '[' <*> ppCom <*> p <*> pleaseSym ']' <*> ppCom)
--       pBrace' p = (Paren'  <$>  spec '{' <*> many pComment <*> p <*> pleaseSym '}' <*> many pComment) -- not yet used

pTree :: P TT (Tree TT)
pTree = (pTuple $ pMany pDTree)
          <|> (pBrack $ pMany pDTree)
--           <|> (pBrace $ pMany pDTree)
          <|> pSType
          <|> pSData
          <|> pDirt
      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

pDirt :: Parser TT (Tree TT)
pDirt = ((Error <$> recoverWith (sym (not . isNoise (\x -> not $ elem x "})]")))))
        <|> (Atom <$> sym (isNoise (\x -> elem x ";,`")))

instance SubTree (Tree TT) where
    type Element (Tree TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Atom t) = f t
              work (Error t) = f t
              work (Block s) = BL.foldMapAfter begin (foldMapToksAfter begin f) s
              work _ = undefined
    foldMapToks f = foldMap (foldMapToks f)

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes point begin _end t0 = trace (show t0) result
    where getStrokes' (Atom t) = one (ts t)
          getStrokes' (Modid t) = one (ts t)
          getStrokes' (Paren' l c g r c')
              | isErrorTok $ tokT r = one (modStroke errorStyle (ts l)) <> foldMap (one . ts) c <> getStrokes' g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) == point || (posnOfs $ tokPosn $ r) == point - 1
               = one (modStroke hintStyle (ts l)) <> foldMap (one . ts) c <> getStrokes' g
                                                  <> one (modStroke hintStyle (ts r))
                                                  <> foldMap (one . ts) c'
              | otherwise  = one (ts l) <> foldMap (one . ts) c <> getStrokes' g
                                        <> one (ts r) <> foldMap (one . ts) c'
          getStrokes' (Error t) = one (modStroke errorStyle (ts t)) -- paint in red
          getStrokes' (Block s) = BL.foldMapAfter begin getStrokesL s
          getStrokes' (Expr g) = getStrokesL g
          getStrokes' (Bin l r) = getStrokes' l <> getStrokes' r
          getStrokes' (KW l r ) = one (ts l) <>  getStrokes' r
          getStrokes' (KW2 l r r') = one (ts l) <> (foldMap (one . ts) r) <> getStrokes' r'
          getStrokes' (Mod m c na c' e w cc' b) = one (ts m) <> com c <> one (ts na) <> com c'
                                                             <> getStrokes' e <> one (ts w)
                                                             <> com cc' <> getStrokes' b
          getStrokes' (Imp m c qu na c' t t') = one (ts m) <> (foldMap (one . ts) c) <> getStrokes' qu
                                                           <> one (ts na) <> (foldMap (one . ts) c')
                                                           <> getStrokes' t  <> getStrokes' t'
          getStrokes' (Typ m  c na exp w b) = one (ts m) <> (foldMap (one . ts) c) <> getStrokes' na 
                                                         <> getStrokes' exp <> getStrokes' w 
                                                         <> getStrokes' b
          getStrokes' (Dat m c na exp eq) = one (ts m) <> (foldMap (one . ts) c) <> getStrokes' na 
                                                        <> getStrokes' exp <> getStrokes' eq                                                        
          getStrokes' (Dat' eq c' b d) = one (ts eq) 
                                              <> (foldMap (one . ts) c')<> getStrokes' b
                                              <> getStrokes' d
          getStrokes' (TypeSig e rest  ) = getStrokes' e <> getStrokesL rest
          getStrokes' (Opt (Just l)) = getStrokes' l
          getStrokes' (Opt Nothing) = getStrokesL []
          getStrokes' (Comm l r) = getStrokes' l <> (foldMap (one . ts) r)
          getStrokes' (Paren l g r)
              | isErrorTok $ tokT r = one (modStroke errorStyle (ts l)) <> getStrokes' g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) == point || (posnOfs $ tokPosn $ r) == point - 1

               = one (modStroke hintStyle (ts l)) <> getStrokes' g <> one (modStroke hintStyle (ts r))
              | otherwise  = one (ts l) <> getStrokes' g <> one (ts r)
          getStrokesL = foldMap getStrokes'
          ts = tokenToStroke
          com r = (foldMap (one . ts) r)
--           tk t | isErrorTok $ tokT t = one (modStroke errorStyle (ts t))
--                | otherwise = one (ts t)
          result = appEndo (getStrokes' t0) []
          one x = Endo (x :)

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f `mappend`)

tokenToAnnot :: TT -> Maybe (Span String)
tokenToAnnot (Tok t len posn) = case tokenToText t of
    Nothing -> Nothing
    Just x -> Just (Span (posnOfs posn) x (posnOfs posn +~ len))
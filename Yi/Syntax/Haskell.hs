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
import Yi.Syntax.BList as BL hiding (many,some)
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
--isNoise _ (Reserved Data) = False
--isNoise _ (Reserved NewType) = False
isNoise _ (Reserved Qualified) = False
isNoise _ (Reserved Hiding) = False
-- isNoise _ (Comment _) = False
-- isNoise _ (Operator _) = False
isNoise _ _ = True

data Tree t
    = Paren t (Tree t) t -- A parenthesized expression
    | Paren' t [t] (Tree t) t [t] -- an extended version of parenthesized expressions that can have following comments
    | Block (BList [Tree t])      -- A list of things separated by layout (as in do; etc.)
    | Atom t
    | Expr [Tree t]
    | KW t (Tree t)
    | KW2 t [t] (Tree t)
    | Bin (Tree t) (Tree t)
    | Error t
    | Opt (Maybe (Tree t))
    | Opt' t [t] -- some optional stuff followed by some comments allmost like Comm
    | Comm (Tree t) [t]
    | Modid t
    | Mod t [t] t [t] (Tree t) t [t] (Tree t)
    | Imp t [t] (Tree t) t [t] (Tree t) (Tree t)
    | Typ (Tree t) (Tree t) (Tree t) (Tree t) (Tree t)
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
    where
      -- | Parse Variables
      pVarId :: Parser TT TT
      pVarId = exact VarIdent

      -- | Parse constructors
      pConId :: Parser TT TT
      pConId = exact ConsIdent

      -- | Parse type variables
      pTyvar :: Parser TT TT
      pTyvar = pVarId

      -- | Parse type constructors
      pTycon :: Parser TT TT
      pTycon = pConId

      -- | Parse type classes
--       pTycls :: Parser TT TT
--       pTycls = pConId

      -- | Parse modules
      pModid :: Parser TT (Tree TT)
      pModid = Modid <$> pConId

      pQvarid :: Parser TT (Tree TT) -- exports like List.map or simply map
      pQvarid = (Atom <$> pConId) <|> (Atom <$> pVarId)

      pQvarsym :: Parser TT (Tree TT)
      pQvarsym = Atom <$> sym isOperator

--       pQconid :: Parser TT (Tree TT)
--       pQconid = Atom <$> pConId

      pQtycon :: Parser TT (Tree TT)
      pQtycon = Atom <$> pTycon

--       pQtycls = pTycls

      pQvar :: Parser TT (Tree TT)
      pQvar = pQvarid  <|> pTup (pCom pQvarsym)

      -- | parse a special symbol
      sym :: (Token -> Bool) ->Parser TT TT
      sym f   = symbol (f . toTok)

      exact :: Token -> Parser TT TT
      exact s = sym (== s)

      spec :: Char -> Parser TT TT
      -- spec '|' = exact (ReservedOp Pipe)
      spec '=' = exact (ReservedOp Equal)
      spec c   = sym (isSpecial [c])

      -- | Create a special character symbol
      newT c = fromT (Special c)

      pleaseSym :: Char -> Parser TT TT
      pleaseSym c = (recoverWith (pure $ newT '!')) <|> spec c

      pleaseB :: Parser TT TT -> Parser TT TT
      pleaseB b   = (recoverWith (pure $ newT '!')) <|> b

      pleaseC ::Parser TT (Tree TT) ->Parser TT (Tree TT)
      pleaseC  c  = (Error <$> recoverWith (pure $ newT '!')) <|> c

      pleaseC' ::Parser TT (Tree TT) ->Parser TT (Tree TT)
      pleaseC' c  = (Error <$> recoverWith (symbol $ const True)) <|> c

      pCppDir    = Atom <$> exact CppDirective
      pCons      = Atom <$> exact ConsIdent
      pVar       = Atom <$> exact VarIdent
      pOp        = Atom <$> sym   isOperator
      pAs        = Atom <$> exact (Reserved As)
      pTyp       = Atom <$> exact (Reserved Type)
      pRArrow    = Atom <$> exact (ReservedOp RightArrow)

      ppCom = many (pComment <|> exact CppDirective)

      pModule :: Parser TT (Tree TT)
      pModule = (b (Reserved Module) ConsIdent ((optional (spec '.')) *> pleaseB (exact (Reserved Where))) pRest)
          where b m na w r
                       = Mod <$> exact m             <*> ppCom
                             <*> pleaseB (exact na)  <*> ppCom
                             <*> pExports
                             <*> w                   <*> ppCom
                             <*> r
                pExports = pOpt $ pTup $ Bin <$> pMany (Bin <$>  pExport <*> pComma) <*> pOpt pExport -- optional trailing comma
                pExport = (optional $ pleaseSym '.') *> ((pCom pQvar)
                                                       <|> (pCom pEModule)
                                                       <|> (Bin <$> ((pCom pQtycon) <|> pTup (pCom pOp))
                                                            <*> pleaseC helper))
                helper = pTup ((pCom $ Atom <$> exact (ReservedOp (OtherOp "..")))
                               <|> (Bin <$> pMany (Bin <$> pCom pQvar <*> pComma)
                                    <*> pOpt pQvar)) -- optional trailing comma
                pRest = ((pBlockOf' (pleaseSym '.' *> (Bin <$> pi <*> pOpt pt))) <|> pOpt pt') <|> (pMany (pTree <|> (Atom <$> spec '.'))) --(pleaseC' $ Atom <$> spec '>')

      pOpt' p  = Opt <$> optional (Opt' <$> p <*> ppCom )
      pOpt p   = Opt <$> optional p
      pCom b   = Comm <$> pleaseC' b <*> many (pComment <|> exact (CppDirective))
      pCom' b  = Comm <$> b          <*> many (pComment <|> exact (CppDirective))
      pComment = sym isComment

      -- | Parse tuple, parenthesis
      pTup :: Parser TT (Tree TT) -> Parser TT (Tree TT)
      pTup p = (Paren'  <$>  spec '(' <*> ppCom
                <*> p <*> pleaseSym ')' <*> ppCom)

      -- | Parse imports
      pImp :: Parser TT (Tree TT)
      pImp = pI (exact (Reserved Import)) (pOpt' $ exact (Reserved Qualified)) (pleaseB $ exact ConsIdent)
          where pI na qu pId
                         = Imp  <$> na       <*> ppCom
                                <*> qu
                                <*> pId      <*> ppCom
                                <*> (pOpt pAs')
                                <*> end
                pAs'     = Bin  <$> pCom pAs <*> pCom pCons
                pImpSpec = pOpt (Bin <$> (pOpt (pCom (Atom <$> exact (Reserved Hiding)))) <*> pleaseC pImpS)
                pImpS    = pTup $ Bin <$> (pMany (Bin <$> pExp' <*>  pComma)) <*> pOpt pExp' -- trailing comma is optional
                pExp'    = Bin <$> (pCom (pCons <|> pVar <|> pOp) <|> pTup (pCom pOp)) <*> pOpt pImpS
                end      = pImpSpec   <* (pleaseSym ('.') <|> pleaseSym ('>'))

      -- | Parse simple types
      pSimpleType :: Parser TT (Tree TT)
      pSimpleType = Typ <$> pCom' pTyp                       <*> (pCom pCons)
                        <*> pMany (pCom' (pCons <|> pVar))   <*> pCom (Atom <$> spec '=')
                        <*> pType <* (pleaseSym '.' <|> pleaseSym '>')

      pType :: Parser TT (Tree TT)
      pType = Bin <$> pSome pAtype <*> pMany (Bin <$> (pCom' pRArrow) <*> pSome pAtype)

      pAtype = pGtycon
               <|> (pCom $ Atom <$> pTyvar)
               <|> (pTup $ pOpt (Bin <$> pType <*> pMany (Bin <$> pComma <*> pType)))
               <|> pBrack' pType

      pGtycon = (pCom pQtycon)
--                 <|> (Bin <$> lSide '(' <*> (pleaseC $ rSide ')')) -- unit type
--                 <|> (Bin <$> lSide '[' <*> (pleaseC $ rSide ']')) -- empty list type
                <|> (pTup pRArrow) -- function constructor
      --  |  (,{,})  (tupling constructors)
--           where lSide l = Atom <$> spec l
--                 rSide r = Atom <$> spec r

      pComma = (Comm <$> (Atom <$> spec ',') <*> many pComment)
      -- Enter "blah" (Comm <$> (Atom <$> spec ',') <*> many pComment)

      isOperator (Operator _)     = True
      isOperator (ReservedOp _)   = True
      isOperator (ConsOperator _) = True
      isOperator _                = False

      -- | Exporting module
      pEModule = KW <$> exact (Reserved Module) <*> pleaseC pModid

      pDecl = Bin <$> (pMany ((Atom <$> pComment) <|> pCppDir))
                  <*> ((pBlockOf' (spec '.' *> (pModule
                                                <|> (Bin <$> pi <*> pOpt pt)))) <|> pOpt pt')

      pt  = Block <$> (pBlocks pDTree)
      pt' = pBlockOf pDTree
      pi  = pSome pImp

      pMany r = Expr <$> many r
      pSome r = Expr <$> some r

      pDTree =
           pBlockOf pDTree
           <|> pTree

      pBlocks p   = (many p) `BL.sepBy` spec '.' -- see HACK above
      pBlockOf p  = (Block <$> (spec '<' *> pBlocks p <* spec '>')) -- see HACK above
      pBlockOf' p = ((spec '<' *> p <* (optional $ spec '>'))) -- see HACK above
      -- also, we discard the empty statements

      pTuple p  = (Paren  <$>  spec '(' <*> p  <*> pleaseSym ')')
      pBrack p  = (Paren  <$>  spec '[' <*> p  <*> pleaseSym ']')
      pBrace p  = (Paren  <$>  spec '{' <*> p  <*> pleaseSym '}')
      pBrack' p = (Paren'  <$>  spec '[' <*> many pComment <*> p <*> pleaseSym ']' <*> many pComment)
--       pBrace' p = (Paren'  <$>  spec '{' <*> many pComment <*> p <*> pleaseSym '}' <*> many pComment) -- not yet used

      pTree :: P TT (Tree TT)
      pTree = (pTuple $ pMany pDTree)
          <|> (pBrack $ pMany pDTree)
          <|> (pBrace $ pMany pDTree)
          <|> pSimpleType
          <|> pDirt
      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

      pDirt = ((Error <$> recoverWith (sym (not . isNoise (\x -> not $ elem x "})]")))))
             <|> (Atom <$> sym (isNoise (\x -> elem x ";,`")))

instance SubTree (Tree TT) where
    type Element (Tree TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Atom t) = f t
              work (Error t) = f t
              work (Block s) = foldMapAfter begin (foldMapToksAfter begin f) s
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
          getStrokes' (Block s) = foldMapAfter begin getStrokesL s
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
          getStrokes' (Typ m na exp w b) = getStrokes' m <> getStrokes' na <> getStrokes' exp
                                                         <> getStrokes' w <> getStrokes' b
          getStrokes' (TypeSig e rest  ) = getStrokes' e <> getStrokesL rest
          getStrokes' (Opt (Just l)) = getStrokes' l
          getStrokes' (Opt Nothing) = getStrokesL []
          getStrokes' (Opt' r c) = one (ts r) <> (foldMap (one . ts) c)
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
{-# LANGUAGE FlexibleInstances, TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}
-- Copyright (c) JP Bernardy 2008
-- Note if the layout of the first line (not comments)
-- is wrong the parser will only parse what is in the blocks given by Layout.hs
module Yi.Syntax.Haskell where

import Prelude ()
import Data.Maybe
import Data.List (delete, filter, union, takeWhile, (\\))
import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
import Yi.Style
import Yi.Syntax.Layout
import Yi.Syntax.Tree
import qualified Yi.Syntax.BList as BL
import Yi.Syntax
import Yi.Prelude
import Prelude ()
import Data.Monoid
import Data.DeriveTH
import Data.Derive.Foldable
import Data.Derive.Data
import Data.Maybe
import Data.Data
import Data.Typeable
import Data.Generics.Schemes

indentScanner :: Scanner (AlexState lexState) (TT)
              -> Scanner (Yi.Syntax.Layout.State Token lexState) (TT)
indentScanner = layoutHandler startsLayout [(Special '(', Special ')'),
                                            (Special '[', Special ']'),
                                            (Special '{', Special '}')]
                         ignoredToken
                         ([(Special '<'), (Special '>'), (Special '.')])
                         isBrace

-- HACK: We insert the Special '<', '>', '.', which do not occur in normal haskell
-- parsing.

isBrace :: TT -> Bool
isBrace (Tok b _ _) = (Special '{') == b

ignoredToken :: TT -> Bool
ignoredToken (Tok t _ (Posn _ _ _)) = isComment t || t == CppDirective

type Tree t = Program t
type PAtom t = Exp t
type Block t = Exp t
type PGuard t = Exp t

-- | A program is some comments followed by a module and a body
data Program t 
    = Program [t] (Maybe (Program t)) -- a program can be just comments
    | ProgMod (PModule t) (Program t)
    | Body [PImport t] (Block t) (Block t)
  deriving (Show, Data, Typeable)

-- | A module
data PModule t = PModule (PAtom t) (PAtom t) (Exp t) (Exp t)
    deriving (Show, Data, Typeable)

-- | Imported things
data PImport t = PImport (PAtom t) (Exp t) (PAtom t) (Exp t) (Exp t)
    deriving (Show, Data, Typeable)

-- | Exp can be expression or declaration
data Exp t
      -- A parenthesized expression with comments
    = Paren (PAtom t) (Exp t) (PAtom t)
      -- Special parenthesis to increase speed of parser
    | SParen (PAtom t) (Exp t)
    | SParen' (Exp t) (PAtom t) (Exp t)
      -- A list of things separated by layout (as in do; etc.)
    | Block (BL.BList [Exp t])
    | PAtom t [t]
    | PFun (Exp t) (Exp t) t [t] (Exp t)
    | Expr [Exp t]
    | KW (PAtom t) (Exp t)
    | PWhere t [t] (Exp t)
    | Bin (Exp t) (Exp t)
       -- an error with comments following so we never color comments in wrong color
    | PError t [t]
      -- rhs that begins with Equal
    | RHS (PAtom t) [Exp t]
    | Opt (Maybe (Exp t))
    | Modid t [t]
    | Op t [t] (Exp t)
    | Context (Exp t) (Exp t) t [t]
    | PType t [t] (Exp t) (Exp t) t [t] (Exp t)
    | PData t [t] (Exp t) (Exp t) (Exp t)
    | PData' t [t] (Exp t) (Exp t)
    | PGuard [PGuard t]
    | PGuard' t (Exp t) t (Exp t)
      -- type constructor
    | TC (Exp t)
      -- type signature
    | TS t [Exp t]
       -- data constructor
    | DC (Exp t)
    | PLet t [t] (Exp t) (Exp t) 
    | PIn t [Exp t]
  deriving (Show, Data, Typeable)

instance SubTree (Exp TT) where
    type Element (Exp TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Paren e e' e'') = work e <> work e' <> work e''
              work (PFun e e' t lt e'') 
                     = work e 
                    <> work e'
                    <> f t
                    <> fold' lt
                    <> work e''
              work (Expr e)     = foldMap work e
              work (KW e e')    = work e <> work e'
              work (PWhere t c e) = f t <> fold' c <> work e
              work (Bin e e')   = work e <> work e'
              work (RHS e l)    = work e <> foldMap work l
              work (Opt (Just t)) = work t
              work (Opt Nothing)  = mempty
              work (Modid t l)    = f t
                                 <> fold' l
              work (Op t l e) = f t
                             <> fold' l
                             <> work e
              work (Context e e' t l) = f t
                                     <> work e
                                     <> work e'
                                     <> fold' l
              work (PType t l e e' t' l' e'') = f t
                                             <> fold' l
                                             <> work e
                                             <> work e'
                                             <> f t'
                                             <> fold' l'
                                             <> work e''
              work (PData t l e e' e'') = f t
                                       <> fold' l
                                       <> work e
                                       <> work e'
                                       <> work e''
              work (PData' t l e e') = f t
                                    <> fold' l
                                    <> work e
                                    <> work e'
              work (PGuard l) = foldMap work l
              work (PGuard' t e t' e') = f t
                                      <> work e
                                      <> f t'
                                      <> work e'
              work (PAtom t c) = f t <> fold' c
              work (PError t c) = f t <> fold' c
              work (TS t e) = f t <> foldMap work e
              work (DC e) = work e
              work (TC e) = work e
              work (PLet t l e e') = f t 
                                  <> fold' l
                                  <> work e
                                  <> work e'
              work (PIn t l) = f t <> foldMap work l
              work (Block s) = BL.foldMapAfter
                                begin (foldMapToksAfter begin f) s
              work a = error $ "Instance SubTree: " ++ show a
              fold' = foldMapToksAfter begin f
    foldMapToks f = foldMap (foldMapToks f)

instance SubTree (Program TT) where
    type Element (Program TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Program m (Just p)) = foldMapToksAfter begin f m <> work p
              work (Program m Nothing) = foldMapToksAfter begin f m
              work (ProgMod m p) = work p
              work (Body i (Block t) (Block t')) = (BL.foldMapAfter
                                begin (foldMapToksAfter begin f) t) 
                                       <> (BL.foldMapAfter
                                       begin (foldMapToksAfter begin f) t')
              work _ = undefined
    foldMapToks f = foldMap (foldMapToks f)

instance SubTree (PImport TT) where
    type Element (PImport TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (PImport at e at' e' e'') = fold' at
                                            <> fold' e
                                            <> fold' at'
                                            <> fold' e'
                                            <> fold' e''
              work _ = undefined
              fold' = foldMapToksAfter begin f
    foldMapToks f = foldMap (foldMapToks f)


type TTT = Exp TT


$(derive makeFoldable ''PImport)
$(derive makeFoldable ''PModule)
$(derive makeFoldable ''Program)

$(derive makeFoldable ''Exp)
instance IsTree Exp where
   subtrees (Paren _ (Expr g) _) = g
   subtrees (RHS _ g)     = g
   subtrees (PWhere _ _ (Block r))  = concat r
   subtrees (Block s)     = concat s
   subtrees (PGuard s)    = s
   subtrees (PLet _ _ (Block s) _)  = concat s
   subtrees (PIn _ ts) = ts
   subtrees (Expr a)      = a
--      subtrees (TypeSig _ a) = a
   subtrees _             = []

$(derive makeTypeable ''Tok)
$(derive makeTypeable ''Token)
$(derive makeTypeable ''ReservedType)
$(derive makeTypeable ''Size)
$(derive makeTypeable ''Posn)
$(derive makeTypeable ''OpType)
$(derive makeTypeable ''CommentType)
$(derive makeData ''Tok)
$(derive makeData ''Token)
$(derive makeData ''Size)
$(derive makeData ''Posn)
$(derive makeData ''CommentType)
$(derive makeData ''OpType)
$(derive makeData ''Point)
$(derive makeData ''ReservedType)

isError' :: Exp TT ->[Exp TT]
isError' n = (listify isE' n)
    where isE' (PError _ _) = True
          isE' _ = False

-- | Search the given list, and return the 1st tree after the given
-- point on the given line.  This is the tree that will be moved if
-- something is inserted at the point.  Precondition: point is in the
-- given line.

-- TODO: this should be optimized by just giving the point of the end
-- of the line
getIndentingSubtree :: [Exp TT] -> Point -> Int -> Maybe (Exp TT)
getIndentingSubtree roots offset line =
    listToMaybe $ [t | (t,posn) <- takeWhile
                   ((<= line) . posnLine . snd) $ allSubTree'sPosn,
--                    -- it's very important that we do a linear search
--                    -- here (takeWhile), so that the tree is evaluated
--                    -- lazily and therefore parsing it can be lazy.
                   posnOfs posn > offset, posnLine posn == line]
    where allSubTree'sPosn = [(t',posn) | root <- roots,
                              t'@(Block _) <- filter (not . null . toList)
                              (getAllSubTrees root),
                             let (tok:_) = toList t',
                             let posn = tokPosn tok]

-- | given a tree, return (first offset, number of lines).
getSubtreeSpan :: Exp TT -> (Point, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last]
              = fmap (tokPosn . assertJust)
                [getFirstElement tree, getLastElement tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x
          assertJust _ = error "assertJust: Just expected"

getExprs :: Program TT -> [Exp TT]
getExprs (ProgMod _ b)     = getExprs b
getExprs (Body _ exp exp') = [exp, exp']
getExprs (Program _ (Just e)) = getExprs e
getExprs _                 = [] -- error "no match"

-- | The parser
parse :: P TT (Tree TT)
parse = pProgram <* eof

-- | Parse a program
pProgram :: Parser TT (Program TT)
pProgram = Program <$> many pComment <*> optional
           (pBlockOf' ((ProgMod <$> pModule
                        <*> pModBody) <|> pBody))

-- | Parse a body that follows a module
pModBody :: Parser TT (Program TT)
pModBody = (Body <$> ((spec '<') *> pImp)
            <*> (((pBol *> pBod) <|> pEmptyBL) <* (spec '>'))
            <*> pBod)
       <|> (Body <$> (spec '.' *>) pImp
            <*> ((pBol *> pBod) <|> pEmptyBL)
            <*> pEmptyBL)
       <|> (Body <$> pure [] <*> pEmptyBL <*> pEmptyBL)
    where pBol  = testNext (\r ->(not $ isJust r) ||
                            not (((flip elem elems) . tokT . fromJust) r))
          pBod  = (Block <$> pBlocks' pDTree)
          elems = [(Special '.'),(Special '<')]

pEmptyBL :: Parser TT TTT
pEmptyBL = Block <$> pure BL.nil

-- | Parse a body of a program
pBody :: Parser TT (Program TT)
pBody = Body <$> pImp <*> (pBol *> (Block <$> pBlocks' pDTree)) <*> pEmptyBL
    where pBol = testNext (\r ->(not $ isJust r) ||
                           not (((flip elem elems) . tokT . fromJust) r))
          elems = [(Special '.'),(Special '<')]

-- Helper functions for parsing follows
-- | Parse Variables
pVarId :: Parser TT (TTT)
pVarId = pAt (exact' [VarIdent, (Reserved Other), (Reserved As)])

-- | Parse constructors
pConId :: Parser TT TT
pConId = exact' [ConsIdent]

-- | Parse modules
pModid :: Parser TT TTT
pModid = Modid <$> pleaseB' pConId <*> pCom

-- | Parse VarIdent and ConsIdent
pQvarid :: Parser TT TTT
pQvarid = pAt (exact' [VarIdent, ConsIdent, (Reserved Other), (Reserved As)])

-- | Parse an operator using please
ppQvarsym :: Parser TT TTT
ppQvarsym = pTup $ ppAt $ sym isOperator

-- | Parse a consident
pQtycon :: Parser TT TTT
pQtycon = pAt pConId

-- | Parse many variables
pVars :: Parser TT TTT
pVars = pMany $ pVarId

-- | Parse an operator
pConop ::Parser TT TTT
pConop = pAt $ sym isOperator

-- | parse a special symbol
sym :: (Token -> Bool) ->Parser TT TT
sym f   = symbol (f . tokT)

-- | Gives a function returning True when any token in the list is parsed
exact :: [Token] -> (Token ->Bool)
exact = flip elem

-- | Parse anything that is in the list
exact' :: [Token] -> Parser TT TT
exact' = sym . exact

-- | Parse special tokens
spec :: Char -> Parser TT TT
spec '|' = exact' [ReservedOp Pipe]
spec '=' = exact' [ReservedOp Equal]
spec c   = sym $ isSpecial [c]

-- | Create a special character symbol
newT :: Char -> TT
newT = tokFromT . Special

-- | Parse a special token using please
pleaseSym :: Char -> Parser TT TT
pleaseSym = ((<|>) pErrN) . spec

pleaseB :: Token -> Parser TT TT
pleaseB r = (pleaseB' . exact') [r]

-- | Parse a Tok using please
pleaseB' :: Parser TT TT -> Parser TT TT
pleaseB' = (<|>) pErrN

-- | Parse a Tree tok using please
pleaseC ::Parser TT TTT ->Parser TT TTT
pleaseC = (<|>) (PError <$> pErrN <*> pure [])

-- | Recover from anything
pErrN :: Parser TT TT
pErrN = (recoverWith $ pure $ newT '!')

-- | Parse anything that is an error
pErr :: Parser TT TTT
pErr = PError <$>
       recoverWith (sym $ not . (\x -> isComment x
                                 ||CppDirective == x))
   <*> pCom

-- | Parse an ConsIdent
ppCons :: Parser TT TTT
ppCons = ppAt $ exact' [ConsIdent]

-- | Parse a keyword
pKW :: Parser TT TT -> Parser TT TTT -> Parser TT TTT
pKW k r = KW <$> pAt k <*> r

-- | Parse an unary operator
pOP :: [Token] -> Parser TT (Exp TT) -> Parser TT (Exp TT)
pOP op r = Op <$> exact' op <*> pCom <*> r

ppOP :: [Token] -> Parser TT (Exp TT) -> Parser TT (Exp TT)
ppOP op r = Op <$> pleaseB' (sym $ flip elem op) <*> pCom <*> r

-- | Parse many comments
pCom ::Parser TT [TT]
pCom = many $ pComment

-- | Parse comment
pComment :: Parser TT TT
pComment = sym (\x -> isComment x || (CppDirective == x))

-- | Parse something thats optional
pOpt :: Parser TT TTT -> Parser TT TTT
pOpt = ((<$>) Opt) . optional

-- | Parse an atom
pAt :: Parser TT TT -> Parser TT TTT
pAt b = PAtom <$> b <*> pCom

-- | Parse an atom using please
ppAt :: Parser TT TT -> Parser TT TTT
ppAt b = pleaseC (PAtom <$> b <*> pCom)

-- | Parse something separated by, with optional ending
pSepBy :: Parser TT TTT -> Parser TT TTT -> Parser TT TTT
pSepBy r p = Bin <$> pMany (Bin <$> r <*> p)
         <*> pOpt r

-- | Parse a comma separator
pComma ::Parser TT TTT
pComma = pAt $ spec ','

-- | Parse a comma using please
ppComma :: Parser TT TTT
ppComma = pAt (pleaseB' $ spec ',')

-- | Parse any operator
isOperator ::Token -> Bool
isOperator (Operator _)     = True
isOperator (ReservedOp _)   = True
isOperator (ConsOperator _) = True
isOperator _                = False

-- End of helper functions Parsing different parts follows

-- | Parse a Module declaration
pModule :: Parser TT (PModule TT)
pModule = (PModule <$> pAt (exact' [Reserved Module])
           <*> pAt (pleaseB ConsIdent)
           <*> pExports
           <*> ((optional $ spec '.') *>
                (Bin <$> pAt (pleaseB $ Reserved Where))
                <*> pMany pErr') <* pEmod)
    where pExports = pOpt (pTup $ pSepBy pExport pComma)
          pExport = ((optional $ spec '.') *>
                     pleaseC (pVarId
                              <|> pEModule
                              <|> (Bin <$> ppQvarsym <*> (DC <$> pOpt helper))
                              <|> (Bin <$> (TC <$> pQtycon) <*> (DC <$> pOpt helper))
                     ))
          helper = pTup $  pleaseC ((pAt $ exact' [ReservedOp $ OtherOp ".."])
                                    <|> (pSepBy pQvarid pComma))
          pEmod = testNext (\r ->(not $ isJust r) ||
                            ((flip elem elems)
                             . tokT . fromJust) r)
          elems = [(Special '.'), (Special '<'), (Special '>')]
          pErr' = PError <$>
                  recoverWith (sym $ not . (\x -> isComment x
                                            ||elem x [CppDirective
                                                     , (Special '<')
                                                     , (Special '>')
                                                     , (Special '.')]))
              <*> pCom

-- | Parse several imports
pImp :: Parser TT [PImport TT]
pImp = many (pImp' 
             <* pEol
             <* (optional $ exact' [(Special '.'),(Special ';')]))
    where pEol :: Parser TT ()
          pEol = testNext (\r ->(not $ isJust r) ||
                           (pEol' r))
          pEol' = (flip elem [(Special '<'),(Special ';'), (Special '.'), (Special '>')])
                . tokT . fromJust
 
-- | Parse one import
-- pImp' :: Parser TT TTT
pImp' :: Parser TT (PImport TT)
pImp' = PImport  <$> pAt (exact' [Reserved Import])
    <*> pOpt (pAt $ exact' [Reserved Qualified])
    <*> pAt (pleaseB ConsIdent)
    <*> pOpt (pKW (exact' [Reserved As]) ppCons)
    <*> (TC <$> pImpSpec)
    where pImpSpec = ((Bin <$> (pKW (exact' [Reserved Hiding]) $
                                pleaseC pImpS) <*> pEnd)
                      <|> (Bin <$> pImpS <*> pEnd)) 
                 <|> pEnd
          pImpS    = (DC <$> ((pTup (pSepBy pExp' pComma))))
          pExp'    = Bin <$> ((pAt $ sym (\x -> (exact [VarIdent, ConsIdent] x)
                                          || isOperator x))
                              <|>  ppQvarsym) <*> pOpt pImpS
          pEnd     = pMany pErr

-- | Parse simple types
pSType :: Parser TT TTT
pSType = PType <$> exact' [Reserved Type]    <*> pCom
             <*> (TC <$> ppCons)             <*> pMany pQvarid
             <*> pleaseB (ReservedOp Equal)  <*> pCom
             <*> (TC <$> pleaseC pType) <* pEol
    where pEol :: Parser TT ()
          pEol = testNext (\r ->(not $ isJust r) ||
                 (pEol'' r))
          pEol'' = (flip elem [ (Special '<')
                              , (Special ';')
                              , (Special '.')
                              , (Special '>')])
                 . tokT . fromJust

-- | Parse typedeclaration
pType :: Parser TT TTT
pType = Block <$> some (pAtype) `BL.sepBy1` (pAt $ exact' [ReservedOp RightArrow])

pSimpleType :: Parser TT TTT
pSimpleType = (Bin <$> (TC <$> ppCons) <*> pMany pQvarid)
          <|> pTup (Bin <$> (TC <$> ppCons) <*> pMany pQvarid)

-- | Parse data declarations
pSData :: Parser TT TTT
pSData = PData <$> exact' [(Reserved Data)] <*> pCom
     <*> pOpt (TC <$> pContext)
     <*> (Bin <$> (TC <$> pSimpleType)   <*> pMany pErr')
     <*> (pOpt (Bin <$> pSData' <*> pMany pErr)) <* pEol
    where pErr' = PError 
              <$> recoverWith (sym $ not .
                               (\x -> isComment x
                                ||(elem x [ CppDirective
                                          , (ReservedOp Equal)
                                          , (Reserved Deriving)])
                               )) <*> pCom
          pEol :: Parser TT ()
          pEol = testNext (\r -> (not $ isJust r) ||
                           pE r)
          pE = (flip elem [(Special ';'), (Special '.'), (Special '>')])
             . tokT . fromJust

-- | Parse second half of the data declaration, if there is one
pSData' :: Parser TT TTT
pSData' = (PData' <$> eqW <*> pCom -- either we have standard data, or we have GADT:s
           <*> ((pleaseC pConstrs)
                <|> (pBlockOf' (Block <$> many pGadt `BL.sepBy1` spec '.')))
           <*> pOpt pDeriving) 
      <|> pDeriving
    where eqW = (exact' [(ReservedOp Equal),(Reserved Where)])

-- | Parse an GADT declaration
pGadt :: Parser TT TTT
pGadt = (Bin <$> (DC <$> pQtycon)
         <*> (ppOP [ReservedOp $ OtherOp "::"]
              (Bin <$> pOpt pContext <*>
               (pType <|> (pOP [Operator "!"] pAtype) <|> pErr))))
    <|>  pErr

-- | Parse a deriving
pDeriving :: Parser TT TTT
pDeriving = TC 
        <$> (pKW (exact' [Reserved Deriving])
             (pleaseC $ pTup 
              (Bin <$> pleaseC pQtycon
               <*> pMany (Bin <$> pComma <*> pleaseC pQtycon))
              <|> pQtycon))

pAtype :: Parser TT TTT
pAtype = pAtype'
     <|> pErr'
    where pErr' = PError 
              <$> recoverWith (sym $ not . 
                               (\x -> isComment x
                                ||elem x [ CppDirective
                                         , (Special '(')
                                         , (Special '[')
                                         , VarIdent
                                         , ConsIdent
                                         , (Reserved Other)
                                         , (Reserved As)]
                               )) <*> pCom

pAtype' :: Parser TT TTT
pAtype' = pQvarid
      <|> (pTup $ pMany (pTree' [(Reserved Data), (Reserved Type)] []))
      <|> (pBrack' $ pMany (pTree' [(Reserved Data), (Reserved Type)] []))

pContext :: Parser TT TTT
pContext = Context <$> pOpt pForAll
       <*> (TC <$> (pClass
                    <|> pTup (pSepBy pClass pComma)))
       <*> pleaseB (ReservedOp DoubleRightArrow) <*> pCom

pClass :: Parser TT TTT
pClass = Bin <$> pQtycon
     <*> (pleaseC pVarId
          <|> pTup (Bin <$> pleaseC pVarId <*> pMany pAtype'))

-- | Parse for all
pForAll :: Parser TT TTT
pForAll = pKW (exact' [Reserved Forall])
          (Bin <$> pVars <*> (ppAt $ exact' [Operator "."]))

pConstrs :: Parser TT TTT
pConstrs = Bin <$> (Bin <$> pOpt pContext <*> pConstr)
       <*> pMany (pOP [ReservedOp Pipe]
                  (Bin <$> pOpt pContext <*> pleaseC pConstr))

pConstr :: Parser TT TTT
pConstr = Bin <$> pOpt pForAll 
      <*> (Bin <$>
           (Bin <$> (DC <$> pAtype) <*>
            (TC <$> pMany (strictF pAtype))) <*> pOpt st)
      <|> Bin <$> lrHs <*> pMany (strictF pAtype)
      <|> pErr
    where lrHs = pOP [Operator "!"] pAtype
          st = pBrace' $ pOpt
               (Bin <$> pFielddecl
                <*> pMany (Bin <$> pComma <*> pFielddecl))

-- | Parse optional strict variables
strictF :: Parser TT TTT -> Parser TT TTT
strictF a = Bin <$> pOpt (pAt $ exact' [Operator "!"]) <*> a

pFielddecl ::Parser TT TTT
pFielddecl = Bin <$> pVars
         <*> pOpt (pOP [ReservedOp $ OtherOp "::"]
                   (pType
                    <|> (pKW (exact' [Operator "!"]) pAtype)
                    <|> pErr))

-- | Exporting module
pEModule ::Parser TT TTT
pEModule = pKW (exact' [Reserved Module]) pModid

-- | Parse a Let expression
pLet :: Parser TT (Exp TT)
pLet = PLet <$> exact' [Reserved Let] <*> pCom
   <*> ((pBlockOf' (Block <$> pBlocks' (pTr el [(Reserved In),(ReservedOp Pipe),(ReservedOp Equal)])))
        <|> ((Expr <$> pure []) <* pEol))
   <*>  pOpt (PAtom <$> exact' [Reserved In] <*> pure [])
    where pEol :: Parser TT ()
          pEol = testNext (\r -> (not $ isJust r) ||
                           (pEol' r))
          pEol' = (flip elem [(Special '>')])
                   . tokT . fromJust
          el = [(Reserved Data),(Reserved Type)]
          pIn = pOpt (PIn <$> exact' [Reserved In] <*> (pTr el [(ReservedOp Pipe),(ReservedOp Equal)]))

-- check if pEq can be used here instead problem with optional ->
pGuard :: Parser TT TTT
pGuard = PGuard 
     <$> some (PGuard' <$> (exact' [ReservedOp Pipe]) <*>
               -- comments are by default parsed after this
               (Expr <$> (pTr' err at))-- can cause yi to crash
               <*> pleaseB' (exact'
                             [(ReservedOp Equal),(ReservedOp RightArrow)]) 
               -- comments are by default parsed after this -- this must be -> if used in case
               <*> (Expr <$> pTr' err' at'))
  where err  = [(Reserved Class),(Reserved Data), (Reserved Type)]
        at   = [(ReservedOp RightArrow),(ReservedOp Equal), (ReservedOp Pipe)]
        err' = [(Reserved Class),(Reserved In),(Reserved Data), (Reserved Type)]
        at'  = [(Reserved In), (ReservedOp Pipe)]

pRHS :: [Token] -> [Token] ->Parser TT TTT
pRHS err at = pGuard 
          <|> pEq err at

pEq :: [Token] -> [Token] -> Parser TT TTT
pEq _ at = RHS <$> (PAtom <$> exact' [ReservedOp Equal] <*> pure [])
       <*> (pTr' err ([(ReservedOp Equal), (ReservedOp Pipe)] `union` at))
  where err  = [ (Reserved In)
               , (ReservedOp Equal)
               , (Reserved Class)
               , (Reserved Data)
               , (Reserved Type)]

pQcon :: Parser TT TTT
pQcon = pTup (pMany pGconsym
              <|> (pAt $ sym isOperator))

pQconid :: Parser TT TTT
pQconid = pQtycon

pGconsym :: Parser TT TTT
pGconsym = pAt (exact' [ReservedOp (OtherOp ":")])
       <|> pQvarid
       <|> pTup (pMany pGconsym)

-- | Parse many of something
pMany ::Parser TT TTT ->Parser TT TTT
pMany r = Expr <$> many r

pSome ::Parser TT TTT ->Parser TT TTT
pSome r = Expr <$> some r

pDTree :: Parser TT [TTT]
pDTree = pTree (\x y -> pure []) err atom
    where err  = [(Reserved In)]
          atom = [(ReservedOp Equal), (ReservedOp Pipe), (Reserved In)]

-- | Parse a some of something separated by the token (Special '.')
pBlocks :: (Parser TT TTT -> Parser TT [TTT])
            -> Parser TT TTT -> Parser TT (BL.BList [TTT])
pBlocks r p = (r p) `BL.sepBy1` spec '.' -- see HACK above

pBlocks' :: Parser TT r -> Parser TT (BL.BList r)
pBlocks' p =  p `BL.sepBy1` spec '.'

-- | Parse a block of some something separated by the tok (Special '.')
pBlockOf :: Parser TT [TTT] -> Parser TT TTT
pBlockOf p  = Block <$> (pBlockOf' $ pBlocks' p) -- see HACK above

-- | Parse something surrounded by (Special '<') and (Special '>')
pBlockOf' :: Parser TT a -> Parser TT a
pBlockOf' p = spec '<' *> p <* spec '>' -- see HACK above

-- | Parse paren expression with comments
pTup :: Parser TT TTT -> Parser TT TTT
pTup p = Paren <$>  pAt (spec '(')
     <*> p <*> ppAt (spec ')')

-- | Parse a Braced expression with comments
pBrace' :: Parser TT TTT -> Parser TT TTT
pBrace' p = Paren  <$>  pAt (spec '{')
        <*> p  <*> ppAt (spec '}')

-- | Parse a Bracked expression with comments
pBrack' :: Parser TT TTT -> Parser TT TTT
pBrack' p = Paren  <$>  pAt (spec '[')
        <*> p <*> ppAt (spec ']')

-- | Parse something that can contain a data, type declaration or a class
pTree :: ([Token] ->[Token] -> Parser TT [TTT]) -> [Token] -> [Token] -> Parser TT [TTT]
pTree opt err at = ((:) <$> beginLine
                <*> (pTypeSig
                     <|> (pTr err (at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))
                     <|> ((:) <$> pAt (exact' [Special ',']) <*> pTree (\x y -> pure []) err at)))
     <|> ((:) <$> pSType <*> pure [])
     <|> ((:) <$> pSData <*> pure [])
     <|> ((:) <$> (PAtom <$> exact' [(Reserved Class)]
                   <*> pure [])
          <*> ((:) <$> (TC <$> (Expr <$> (pTr' err $ delete (ReservedOp Pipe) at))) <*> pure []))
     <|> opt err at
    where beginLine = (pTup' (Expr <$> pTr err at))
                  <|> (PAtom <$> sym (flip notElem $ isNoise errors) <*> pure [])
                  <|> (PError <$> recoverWith
                       (sym $ flip elem $ isNoiseErr errors) <*> pure [])
          errors = [ (Reserved Class)
                   , (ReservedOp Pipe)
                   , (ReservedOp Equal)
                   , (Reserved Let)
                   , (Reserved In)
                   , (Reserved Where)
                   , (Special '{')
                   , (Special '[')]

-- | The pWBlock describes what extra things are allowed in a where claus
pWBlock err at = pure []
     <|> ((:) <$> (pBrack $ Expr <$> pTr' err (at \\ [(Special ','), (ReservedOp Pipe),(ReservedOp Equal)]))
          <*> (pTr err $ at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))
     <|> ((:) <$> (pBrace $ Expr <$> (pTr' err (at \\ [(Special ','),(ReservedOp Pipe),(ReservedOp Equal)])))
          <*> (pTr err $ at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))


-- | Parse something not containing a Type, Data declaration or a class kw but parse a where
pTr :: [Token] -> [Token] -> Parser TT [TTT]
pTr err at
    = pure []
  <|> ((:) <$> (pTree' (noiseErr \\ [(ReservedOp Pipe),(ReservedOp Equal)]) at
                <|> pBlockOf (pTr err (at \\ [(Special ',')])))
       <*> pTr err (at \\ [(ReservedOp (OtherOp "::")),(Special ','),(ReservedOp RightArrow)]))
  <|> ((:) <$> pRHS err (at \\ [(Special ','),(ReservedOp (OtherOp "::"))]) <*> pure []) -- guard or equal
  <|> ((:) <$> (PWhere <$> exact' [Reserved Where] <*> many pComment <*> pleaseC (pBlockOf $ pTree pWBlock err' atom'))
       <*> pTree (\x y -> pure []) err' atom')
    where err' = [(Reserved In)]
          atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

-- | Parse something where guards are not allowed
pTr' :: [Token] -> [Token] -> Parser TT [TTT]
pTr' err at = pure []
          <|> ((:) <$> (pTree' ([ReservedOp Pipe] `union` err) at
                        <|> (pBlockOf (pTr err (([(ReservedOp Equal), (ReservedOp Pipe)] `union` at) 
                                                \\ [(ReservedOp (OtherOp "::")),(ReservedOp RightArrow)]))))
               <*> pTr' err at)
          <|> ((:) <$> (PWhere <$> exact' [Reserved Where] <*> many pComment <*> pleaseC (pBlockOf $ pTree pWBlock err' atom'))
              <*> pTr' err at)
    where err' = [(Reserved In)]
          atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

-- | Parse a Tree' of expressions
pTree' ::[Token] -> [Token] -> Parser TT TTT
pTree' err at
    = (pTup' (Expr <$> (pTr err (at \\ [Special ',']))))
  <|> (pBrack (Expr <$> (pTr' err (at \\ [(Special ','), (ReservedOp Pipe),(ReservedOp Equal)]))))
  <|> (pBrace (Expr <$> (pTr' err (at \\ [(Special ','),(ReservedOp Pipe),(ReservedOp Equal)]))))
  <|> pLet
  <|> (PError <$> recoverWith
       (sym $ flip elem $ (isNoiseErr err)) <*> pure [])
  <|> (PAtom <$> sym (flip notElem $ (isNoise at)) <*> pure [])
      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

-- | Parse a typesignature 
-- not finished yet!!
pTypeSig :: Parser TT [TTT]
pTypeSig = ((:) <$> (TS <$>  exact' [ReservedOp (OtherOp "::")]
                     <*> (pTr noiseErr []) <* pE) <*> pure [])
    where pE = testNext (\r ->(not $ isJust r) ||
                 (pEol'' r))
          pEol'' = ((flip elem [(Special ';'), (Special '>'), (Special '<'), (Special '.'), (Special ')')])
                 . tokT . fromJust)

-- | A list of keywords that usually should be an error
noiseErr :: [Token]
noiseErr = [(Reserved Class)
           , (ReservedOp Pipe)
           , (Reserved In)
           , (Reserved Data)
           , (Reserved Type)]

-- | A list of Keywords that usually are not allowed as atoms
noiseAt :: [Token]
noiseAt = [(ReservedOp Pipe), (Reserved In)]

-- | List of things that allways should be parsed as errors
isNoiseErr :: [Token] -> [Token]
isNoiseErr r
    = [ (Reserved Module)
      , (Reserved Import)
      , (Special '}')
      , (Special ')')
      , (Special ']')] ++ r

-- | List of things that never should be parsed as an atom
isNoise :: [Token] -> [Token]
isNoise r
    = [ (Reserved Let)
      , (Reserved Class)
      , (Reserved Module)
      , (Reserved Import)
      , (Reserved Type)
      , (Reserved Data)
      , (Reserved Where)] ++ (fmap Special "()[]{}<>.") ++ r

-- | Parse an atom witout comments
pEAtom :: Parser TT TT -> Parser TT TTT
pEAtom r = PAtom <$> r <*> pure []

-- | Parse paren expression with comments
pTup' :: Parser TT TTT -> Parser TT TTT
pTup' p = (Paren <$> pEAtom (spec '(')
          <*> p <*> pEAtom (pleaseSym ')'))

-- | Parse a Braced expression with comments
pBrace :: Parser TT TTT -> Parser TT TTT
pBrace p  = (Paren  <$>  pEAtom (spec '{') 
              <*> p  <*> pEAtom (pleaseSym '}'))

-- | Parse a Bracked expression with comments
pBrack :: Parser TT TTT -> Parser TT TTT
pBrack p = (Paren  <$>  pEAtom (spec '[')  
             <*> p <*> pEAtom (pleaseSym ']'))

-- Stroke the program

-- TODO: (optimization) make sure we take in account the begin, so we don't return useless strokes
getStrokes :: Point -> Point -> Point -> Tree TT -> [Stroke]
getStrokes point begin _end t0 = trace (show t0) result
    where result = appEndo (getStrokeProg point begin _end t0) []

-- | getStroke Program
getStrokeProg ::  Point -> Point -> Point -> Tree TT -> Endo [Stroke]
getStrokeProg point begin _end prog
    = case prog of
        (Program c m)
            ->com c <> funPr m
        (ProgMod m body)
            -> getStrokeMod point begin _end m
            <> getStrokeProg point begin _end body
        (Body imps exps exps') 
            -> funImp imps
            <> getStr tkDConst point begin _end exps
            <> getStr tkDConst point begin _end exps'
  where funPr (Just pr)    = getStrokeProg point begin _end pr
        funPr Nothing      = foldMap id []
        funImp imps        = foldMap (getStrokeImp point begin _end) imps

-- | Get strokes Module for module
getStrokeMod :: Point -> Point -> Point -> PModule TT -> Endo [Stroke]
getStrokeMod point begin _end (PModule m na e w)
              | isErrN na || isErrN w
                     = paintAtom errorStyle m
                    <> getStr tkImport point begin _end na <> getStrokes' e
                    <> getStrokes' w
              | otherwise = getStrokes' m <> getStr tkImport point begin _end na
                         <> getStrokes' e <> getStrokes' w
    where getStrokes' r = getStr tkDConst point begin _end r

-- | Get strokes for Imports
getStrokeImp ::  Point -> Point -> Point -> PImport TT -> Endo [Stroke]
getStrokeImp point begin _end (PImport m qu na t t')
              | isErrN t' || isErrN na || isErrN t
                          = paintAtom errorStyle m <> paintQu qu
                         <> getStr tkImport point begin _end na <> paintAs t  <> paintHi t'
              | otherwise = getStrokes' m <> paintQu qu
                         <> getStr tkImport point begin _end na <> paintAs t  <> paintHi t'
    where getStrokes' r = getStr tkDConst point begin _end r
          paintAs (Opt (Just (KW (PAtom n c) tw)))
              = (one $ (fmap (const keywordStyle) . tokToSpan) n) <> com c
             <> getStr tkImport point begin _end tw
          paintAs a = getStrokes' a
          paintQu (Opt (Just ((PAtom n c)))) = (one $ (fmap (const keywordStyle) . tokToSpan) n) <> com c
          paintQu a = getStrokes' a
          paintHi (Bin (KW (PAtom n c) tw) r) = (one $ (fmap (const keywordStyle) . tokToSpan) n) 
                                             <> com c <> getStr tkImport point begin _end tw
                                             <> getStrokes' r
          paintHi a = getStrokes' a

-- | Get strokes for expressions and declarations
getStr ::(TT -> Endo [Stroke]) -> Point -> Point -> Point -> Exp TT -> Endo [Stroke]
getStr tk point begin _end t0 = getStrokes' t0
    where getStrokes' ::Exp TT -> Endo [Stroke]
          getStrokes' (PAtom t c) = tk t <> com c
          getStrokes' (TS col ts') = tk col <> foldMap (getStr tkTConst point begin _end) ts'
          getStrokes' (Modid t c) = tkImport t <> com c
          getStrokes' (Paren (PAtom l c) g (PAtom r c'))
              | isErr r = errStyle l <> getStrokes' g
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) ==
                    point || (posnOfs $ tokPosn $ r) == point - 1
               = pStyle hintStyle l <> com c <> getStrokes' g
                      <> pStyle hintStyle r <> com c'
              | otherwise  = tk l <> com c <> getStrokes' g
                                  <> tk r <> com c'
          getStrokes' (Paren (PAtom l c) g e@(PError _ _))
              = errStyle l <> com c <> getStrokes' g <> getStrokes' e
          getStrokes' (PError t c) = errStyle t <> com c
          getStrokes' (Block s) = BL.foldMapAfter begin getStrokesL s
          getStrokes' (PFun f args s c rhs)
              | isErrN args || isErr s
              = foldMap errStyle f <> getStrokes' args
              | otherwise = getStrokes' f <> getStrokes' args
                          <> tk s <> com c <> getStrokes' rhs
          getStrokes' (Expr g) = getStrokesL g
          getStrokes' (PWhere c c' exp) = tk c <> com c' <> getStrokes' exp
          getStrokes' (RHS eq g) = getStrokes' eq <> getStrokesL g
--           getStrokes' (RHS eq g) = paintAtom errorStyle eq <> foldMap errStyle (Expr g) -- will color rhs functions red
          getStrokes' (Bin l r) = getStrokes' l <> getStrokes' r
          getStrokes' (KW l r') = getStrokes' l <> getStrokes' r'
          getStrokes' (Op op c r') = tk op <> com c <> getStrokes' r'
          getStrokes' (PType m c na exp eq c' b)
              | isErrN b ||isErrN na || isErr eq
                          = errStyle m <> com c  <> getStrokes' na
                                       <> getStrokes' exp <> tk eq
                                       <> com c <> getStrokes' b
              | otherwise = tk m <> com c <> getStrokes' na
                                       <> getStrokes' exp <> tk eq
                                       <> com c' <> getStrokes' b
          getStrokes' (PData m c na exp eq)
              | isErrN exp || isErrN na ||isErrN eq
                           = errStyle m <> com c <> getStrokes' na
                                        <> getStrokes' eq
              | otherwise = tk m <> com c <> getStrokes' na
                         <> getStrokes' exp <> getStrokes' eq
          getStrokes' (PData' eq c' b d) =
                tk eq <> com c' <> getStrokes' b
                            <> getStrokes' d
          getStrokes' (PLet l c expr i) =
                tk l <> com c <> getStrokes' expr <> getStrokes' i
          getStrokes' (PIn t l) = tk t <> getStrokesL l
          getStrokes' (Opt (Just l)) =  getStrokes' l
          getStrokes' (Opt Nothing) = getStrokesL []
          getStrokes' (Context fAll l arr c) =
                getStrokes' fAll <> getStrokes' l <> tk arr <> com c
          getStrokes' (TC l) = getStr tkTConst point begin _end l
          getStrokes' (DC (PAtom l c)) = tkDConst l <> com c
          getStrokes' (DC r) = getStrokes' r -- do not color operator dc
          getStrokes' (PGuard ls) = getStrokesL ls
          getStrokes' (PGuard' t e t' e')
              | isErrN e ||isErrN e' ||isErr t'
              = errStyle t <> getStrokes' e <> tk t' <> getStrokes' e'
              | otherwise
              = one (ts t) <> getStrokes' e <> tk t' <> getStrokes' e'
          getStrokes' (SParen (PAtom l c) (SParen' g (PAtom r c') e))
              | isErr r = errStyle l <> getStrokes' g <> getStrokes' e
              -- left paren wasn't matched: paint it in red.
              -- note that testing this on the "Paren" node actually forces the parsing of the
              -- right paren, undermining online behaviour.
              | (posnOfs $ tokPosn $ l) ==
                    point || (posnOfs $ tokPosn $ r) == point - 1
               = pStyle hintStyle l <> com c <> getStrokes' g
                      <> pStyle hintStyle r <> com c' <> getStrokes' e
              | otherwise  = tk l <> com c <> getStrokes' g
                                  <> tk r <> com c' <> getStrokes' e
          getStrokes' a = error (show a)
          getStrokesL = foldMap getStrokes'

-- Stroke helpers follows

tokenToAnnot :: TT -> Maybe (Span String)
tokenToAnnot (Tok t len posn) = case tokenToText t of
    Nothing -> Nothing
    Just x -> Just (Span (posnOfs posn) x (posnOfs posn +~ len))

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

isErrN :: (Exp TT) -> Bool
isErrN t = (any isErr t) 
        || (not $ null $ isError' t)

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
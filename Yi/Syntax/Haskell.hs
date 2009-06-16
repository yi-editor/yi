{-# LANGUAGE FlexibleInstances, TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}
-- Copyright (c) JP Bernardy 2008
-- Note if the layout of the first line (not comments)
-- is wrong the parser will only parse what is in the blocks given by Layout.hs
module Yi.Syntax.Haskell ( Program (..)
                         , PModule (..)
                         , PImport (..)
                         , Exp (..)
                         , Tree
                         , parse
                         , indentScanner
                         , getExprs
                         ) where

import Prelude ()
import Data.Maybe
import Data.List (filter, union, takeWhile, (\\))
import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.Haskell
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
  deriving (Show)

-- | A module
data PModule t = PModule (PAtom t) (PAtom t) (Exp t) (Exp t)
    deriving (Show)

-- | Imported things
data PImport t = PImport (PAtom t) (Exp t) (PAtom t) (Exp t) (Exp t)
    deriving (Show)

-- | Exp can be expression or declaration
data Exp t
      -- Top declarations
      -- type signature same as the TC constructor
    = TS t [Exp t]
    | PType (PAtom t) (Exp t) (Exp t) (PAtom t) (Exp t)
    | PData (PAtom t) (Exp t) (Exp t) (Exp t)
    | PData' (PAtom t) (Exp t) (Exp t)
      -- keyword, Opt scontext, tycls, tyvar, Opt where
    | PClass (PAtom t) (Exp t) (Exp t) (Exp t) (Exp t)
      -- keyword, Opt scontext, tycls, Inst, where
    | PInstance (PAtom t) (Exp t) (Exp t) (Exp t) (Exp t)
      -- declarations and parts of them follow
      -- A parenthesized expression with comments
    | Paren (PAtom t) [Exp t] (PAtom t)
      -- Special parenthesis to increase speed of parser
    | SParen (PAtom t) (Exp t)
    | SParen' (Exp t) (PAtom t) (Exp t)
      -- A list of things separated by layout (as in do; etc.)
    | Block (BL.BList [Exp t])
    | PAtom t [t]
    | Expr [Exp t]
    | PWhere (PAtom t) (Exp t)
    | Bin (Exp t) (Exp t)
       -- an error with comments following so we never color comments in wrong
       -- color. The error has an extra token, the Special '!' token to indicate
       -- that it contains an error
    | PError t t [t]
      -- rhs that begins with Equal
    | RHS (PAtom t) [Exp t]
    | Opt (Maybe (Exp t))
    | Modid t [t]
    | Context (Exp t) (Exp t) (PAtom t)
    | PGuard [PGuard t]
    -- the PAtom in PGuard' does not contain any comments
    | PGuard' t (Exp t) (PAtom t) (Exp t)
      -- type constructor is just a wrapper to indicate which highlightning to use
    | TC (Exp t)
      -- data constructor same as with the TC constructor
    | DC (Exp t)
    | PLet (PAtom t) (Exp t) (Exp t)
    | PIn t [Exp t] -- not currently used
  deriving (Show)

instance SubTree (Exp TT) where
    type Element (Exp TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Paren e e' e'') = work e <> foldMap work e' <> work e''
              work (Expr e)     = foldMap work e
              work (PWhere e' e) = work e' <> work e
              work (Bin e e')   = work e <> work e'
              work (RHS e l)    = work e <> foldMap work l
              work (Opt (Just t)) = work t
              work (Opt Nothing)  = mempty
              work (Modid t l)    = f t
                                 <> fold' l
              work (Context e e' t) = work e
                                   <> work e'
                                   <> work t
              work (PType kw e e' exp e'') = work kw
                                             <> work e
                                             <> work e'
                                             <> work exp
                                             <> work e''
              work (PData kw e e' e'') = work kw
                                       <> work e
                                       <> work e'
                                       <> work e''
              work (PData' eq e e') = work eq
                                    <> work e
                                    <> work e'
              work (PGuard l) = foldMap work l
              work (PGuard' t e t' e') = f t
                                      <> work e
                                      <> work t'
                                      <> work e'
              work (PAtom t c)  = f t <> fold' c
              work (PError t' t c) = f t' <> f t <> fold' c
              work (TS t e) = f t <> foldMap work e
              work (DC e) = work e
              work (TC e) = work e
              work (PLet t e e') = work t
                                <> work e
                                <> work e'
              work (PIn t l) = f t <> foldMap work l
              work (Block s) = BL.foldMapAfter
                                begin (foldMapToksAfter begin f) s
              work (PClass e e' e'' exp exp') = work e <> work e' <> work e''
                                             <> work exp <> work exp'
              work (PInstance e e' exp exp' e'' ) = work e <> work e'
                                                 <> work exp <> work exp' <> work e''
              work a = error $ "Instance SubTree: " ++ show a
              fold' = foldMapToksAfter begin f
    foldMapToks f = foldMap (foldMapToks f)

instance SubTree (Program TT) where
    type Element (Program TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (Program m (Just p)) = foldMapToksAfter begin f m <> work p
              work (Program m Nothing) = foldMapToksAfter begin f m
              work (ProgMod _ p) = work p
              work (Body _ (Block t) (Block t')) = (BL.foldMapAfter
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
              fold' = foldMapToksAfter begin f
    foldMapToks f = foldMap (foldMapToks f)

$(derive makeFoldable ''PImport)
$(derive makeFoldable ''PModule)
$(derive makeFoldable ''Program)

$(derive makeFoldable ''Exp)
instance IsTree Exp where
   subtrees tree = case tree of
       (Paren _ g _)  -> g
       (RHS _ g)      -> g
       (PWhere _ r) -> subtrees r
       (Block s)      -> concat s
       (PGuard s)     -> s
       (PLet _ s _) -> subtrees s
       (PIn _ ts)     -> ts
       (Expr a)       -> a
       _              -> []

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
pProgram = Program <$> pComments <*> optional
           (pBlockOf' (ProgMod <$> pModule
                       <*> pModBody <|> pBody))

-- | Parse a body that follows a module
pModBody :: Parser TT (Program TT)
pModBody = ((exact [startBlock]) *>
            ((Body <$> pImports
              <*> ((pBol *> pBod) <|> pEmptyBL) <* (exact [Special '>'])
              <*> pBod) <|>
             (Body <$> noImports <*> ((pBod <|> pEmptyBL) <* (exact [Special '>']))
              <*> pBod)))
       <|> ((exact [nextLine]) *> pBody)
       <|> Body <$> pure [] <*> pEmptyBL <*> pEmptyBL
    where pBol  = pTestTok elems
          pBod  = Block <$> pBlocks pDTree
          elems = [nextLine, startBlock]

pEmptyBL :: Parser TT (Exp TT)
pEmptyBL = Block <$> pure BL.nil

-- | Parse a body of a program
pBody :: Parser TT (Program TT)
pBody = Body <$> noImports <*> (Block <$> pBlocks pDTree) <*> pEmptyBL
    <|> Body <$> pImports <*> ((pTestTok elems *> (Block <$> pBlocks pDTree)) <|> pEmptyBL) <*> pEmptyBL
    where elems = [nextLine, startBlock]

noImports = (notNext [Reserved Import]) *> pure []
    where notNext f = testNext (\r -> (not $ isJust r) || (not . elem ((tokT . fromJust) r)) f)

-- Helper functions for parsing follows
-- | Parse Variables
pVarId :: Parser TT ((Exp TT))
pVarId = pAtom [VarIdent, (Reserved Other), (Reserved As)]

-- | Parse VarIdent and ConsIdent
pQvarid :: Parser TT (Exp TT)
pQvarid = pAtom [VarIdent, ConsIdent, (Reserved Other), (Reserved As)]

-- | Parse an operator using please
pQvarsym :: Parser TT (Exp TT)
pQvarsym = pParen ((:) <$> (please $ PAtom <$> sym isOperator <*> pComments) <*> pure []) pComments

-- | Parse any operator
isOperator :: Token -> Bool
isOperator (Operator _)     = True
isOperator (ReservedOp _)   = True
isOperator (ConsOperator _) = True
isOperator _                = False

-- | Parse a consident
pQtycon :: Parser TT (Exp TT)
pQtycon = pAtom [ConsIdent]

-- | Parse many variables
pVars :: Parser TT (Exp TT)
pVars = pMany pVarId

-- | Parse a nextline token (the nexLine token is inserted by Layout.hs)
nextLine :: Token
nextLine = Special '.'

-- | Parse a startBlock token
startBlock :: Token
startBlock = Special '<'

-- | Parse a endBlock token
endBlock :: Token
endBlock = Special '>'


-- | parse a special symbol
sym :: (Token -> Bool) -> Parser TT TT
sym f = symbol (f . tokT)

-- | Parse anything that is in the list
exact :: [Token] -> Parser TT TT
exact = sym . (flip elem)

-- | Create a special character symbol
newT :: Char -> TT
newT = tokFromT . Special

-- | Parse a Tree tok using please
please :: Parser TT (Exp TT) -> Parser TT (Exp TT)
please = (<|>) (PError <$> (recoverWith $ pure $ newT '!')
                <*> pure (newT '!')
                <*> pure [])

-- | Parse anything that is an error
pErr :: Parser TT (Exp TT)
pErr = PError <$> recoverWith (sym $ not . (\x -> isComment x
                                            || CppDirective == x))
   <*> pure (newT '!')
   <*> pComments

-- | Parse an ConsIdent
ppCons :: Parser TT (Exp TT)
ppCons = ppAtom [ConsIdent]

-- | Parse a keyword
pKW :: [Token] -> Parser TT (Exp TT) -> Parser TT (Exp TT)
pKW k r = Bin <$> pAtom k <*> r

-- | Parse an unary operator with and without using please
pOP, ppOP :: [Token] -> Parser TT (Exp TT) -> Parser TT (Exp TT)
pOP op r = Bin <$> pAtom op <*> r

ppOP op r = Bin <$> ppAtom op <*> r

-- | Parse comments
pComments :: Parser TT [TT]
pComments = many $ sym $ \x -> isComment x || CppDirective == x

-- | Parse something thats optional
pOpt :: Parser TT (Exp TT) -> Parser TT (Exp TT)
pOpt = ((<$>) Opt) . optional

-- | Parse an atom with, and without using please
pAtom, ppAtom :: [Token] -> Parser TT (Exp TT)
pAtom = flip pCAtom pComments

ppAtom = please . pAtom

-- | Parse an atom with optional comments
pCAtom :: [Token] -> Parser TT [TT] -> Parser TT (Exp TT)
pCAtom r c = PAtom <$> exact r <*> c

-- | Parse something separated by, with optional ending
pSepBy :: Parser TT (Exp TT) -> Parser TT (Exp TT) -> Parser TT [Exp TT]
pSepBy r p = pure []
          <|> (:) <$> r <*> (pSep r p <|> pure [])
          <|> (:) <$> p <*> pure [] -- optional ending comma
    where pTok r p = (:) <$> r <*> (pure [] <|> pSep r p)
          pSep r p = (:) <$> p <*> (pure [] <|> pTok r p)

-- | Parse a comma separator
pComma :: Parser TT (Exp TT)
pComma = pAtom [Special ',']

-- End of helper functions Parsing different parts follows

-- | Parse a Module declaration
pModule :: Parser TT (PModule TT)
pModule = PModule <$> pAtom [Reserved Module]
      <*> ppAtom [ConsIdent]
      <*> pExports
      <*> ((optional $ exact [nextLine]) *>
           (Bin <$> ppAtom [Reserved Where])
           <*> pMany pErr) <* pTestTok elems
    where pExports = pOpt (pParen (pSepBy pExport pComma) pComments)
          pExport = ((optional $ exact [nextLine]) *>
                     (pVarId
                      <|> pEModule
                      <|> (Bin <$> pQvarsym <*> (DC <$> pOpt helper))
                      <|> (Bin <$> (TC <$> pQtycon) <*> (DC <$> pOpt helper))
                     ))
          helper = pParen ((:) <$> 
                           (please (pAtom [ReservedOp $ OtherOp ".."]
                                    <|> (Expr <$> pSepBy pQvarid pComma))) <*> pure [])
                   pComments
          elems = [nextLine, startBlock, endBlock]
          pErr = PError <$>
                 recoverWith (sym $ not . (\x -> isComment x
                                           ||elem x [CppDirective
                                                    , startBlock
                                                    , endBlock
                                                    , nextLine]))
             <*> pure (newT '!')
             <*> pComments

-- | Check if next token is in given list
pTestTok :: [Token] -> Parser TT ()
pTestTok f = testNext (\r -> (not $ isJust r) || elem ((tokT . fromJust) r) f)

-- | Parse several imports
pImports :: Parser TT [PImport TT]
pImports = many (pImport
                 <* pTestTok pEol
                 <* (optional $ exact [nextLine,(Special ';')]))
        where pEol = [startBlock, (Special ';'), nextLine, endBlock]
 
-- | Parse one import
pImport :: Parser TT (PImport TT)
pImport = PImport  <$> pAtom [Reserved Import]
      <*> pOpt (pAtom [Reserved Qualified])
      <*> ppAtom [ConsIdent]
      <*> pOpt (pKW [Reserved As] ppCons)
      <*> (TC <$> pImpSpec)
        where pImpSpec = ((Bin <$> (pKW [Reserved Hiding] $
                                    please pImpS) <*> pMany pErr)
                          <|> (Bin <$> pImpS <*> pMany pErr))
                     <|> pMany pErr
              pImpS    = DC <$> (pParen ((pSepBy pExp' pComma)) pComments)
              pExp'    = Bin <$> ((PAtom <$> sym (\x -> (flip elem [VarIdent, ConsIdent] x)
                                                  || isOperator x) <*> pComments)
                                  <|>  pQvarsym) <*> pOpt pImpS

-- | Parse simple types
pSType :: Parser TT (Exp TT)
pSType = PType <$> pAtom [Reserved Type]
     <*> (TC <$> ppCons) <*> pMany pQvarid
     <*> ppAtom [ReservedOp Equal]
     <*> (TC <$> please pTypeRhs) <* pTestTok pEol
    where pEol =[ startBlock
                , (Special ';')
                , nextLine
                , endBlock]

-- | Parse typedeclaration inside something
pTypeRhs :: Parser TT (Exp TT)
pTypeRhs = Block <$> some pAtype `BL.sepBy1` pAtom [ReservedOp RightArrow]

pSimpleType :: Parser TT (Exp TT)
pSimpleType = (Bin <$> (TC <$> ppCons) <*> pMany pQvarid)
          <|> (pParen ((:) <$> (TC <$> ppCons) <*> many pQvarid) pComments)

-- | Parse data declarations
pSData :: Parser TT (Exp TT)
pSData = PData <$> pAtom [(Reserved Data)]
     <*> pOpt (TC <$> pContext)
     <*> (Bin <$> (TC <$> pSimpleType)   <*> pMany pErr')
     <*> (pOpt (Bin <$> pSData' <*> pMany pErr)) <* pTestTok pEol
    where pErr' = PError
              <$> recoverWith (sym $ not .
                               (\x -> isComment x
                                ||(elem x [ CppDirective
                                          , (ReservedOp Equal)
                                          , (Reserved Deriving)])
                               )) 
              <*> pure (newT '!')
              <*> pComments
          pEol = [(Special ';'), nextLine, endBlock]

-- | Parse second half of the data declaration, if there is one
pSData' :: Parser TT (Exp TT)
pSData' = (PData' <$> pAtom eqW -- either we have standard data, or we have GADT:s
           <*> (please pConstrs
                <|> pBlockOf' (Block <$> many pGadt `BL.sepBy1` exact [nextLine]))
           <*> pOpt pDeriving)
      <|> pDeriving
    where eqW = [(ReservedOp Equal),(Reserved Where)]

-- | Parse an GADT declaration
pGadt :: Parser TT (Exp TT)
pGadt = (Bin <$> (DC <$> pQtycon)
         <*> (ppOP [ReservedOp $ OtherOp "::"]
              (Bin <$> pOpt pContext <*>
               (pTypeRhs <|> (pOP [Operator "!"] pAtype) <|> pErr))))
    <|>  pErr

-- | Parse a deriving
pDeriving :: Parser TT (Exp TT)
pDeriving = TC
        <$> (pKW [Reserved Deriving]
             (please (pParen
              (((:) <$> please pQtycon
                <*> many (Bin <$> pComma <*> please pQtycon))) pComments
                      <|> pQtycon)))

pAtype :: Parser TT (Exp TT)
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
                               ))
              <*> pure (newT '!')
              <*> pComments

pAtype' :: Parser TT (Exp TT)
pAtype' = pQvarid
      <|> pParen (many $ pTree' [(Reserved Data), (Reserved Type)] []) pComments
      <|> pBrack (many $ pTree' [(Reserved Data), (Reserved Type)] []) pComments

pContext :: Parser TT (Exp TT)
pContext = Context <$> pOpt pForAll
       <*> (TC <$> (pClass'
                    <|> pParen (pSepBy pClass' pComma) pComments))
       <*> ppAtom [ReservedOp DoubleRightArrow]
        where pClass' :: Parser TT (Exp TT)
              pClass' = Bin <$> pQtycon
                   <*> (please pVarId
                        <|> pParen ((:) <$> please pVarId <*> many pAtype') pComments)

-- | Parse for all
pForAll :: Parser TT (Exp TT)
pForAll = pKW [Reserved Forall]
          (Bin <$> pVars <*> (ppAtom [Operator "."]))

pConstrs :: Parser TT (Exp TT)
pConstrs = Bin <$> (Bin <$> pOpt pContext <*> pConstr)
       <*> pMany (pOP [ReservedOp Pipe]
                  (Bin <$> pOpt pContext <*> please pConstr))

pConstr :: Parser TT (Exp TT)
pConstr = Bin <$> pOpt pForAll
      <*> (Bin <$>
           (Bin <$> (DC <$> pAtype) <*>
            (TC <$> pMany (strictF pAtype))) <*> pOpt st)
      <|> Bin <$> lrHs <*> pMany (strictF pAtype)
      <|> pErr
    where lrHs = pOP [Operator "!"] pAtype
          st = pBrace ((:) <$> (pOpt
                                $ Bin <$> pFielddecl
                                <*> pMany (Bin <$> pComma <*> pFielddecl))
                      <*> pure [])
               pComments

-- | Parse optional strict variables
strictF :: Parser TT (Exp TT) -> Parser TT (Exp TT)
strictF a = Bin <$> pOpt (pAtom [Operator "!"]) <*> a

pFielddecl ::Parser TT (Exp TT)
pFielddecl = Bin <$> pVars
         <*> pOpt (pOP [ReservedOp $ OtherOp "::"]
                   (pTypeRhs
                    <|> (pKW [Operator "!"] pAtype)
                    <|> pErr))

-- | Exporting module
pEModule ::Parser TT (Exp TT)
pEModule = pKW [Reserved Module] $ please (Modid <$> (exact [ConsIdent]) <*> pComments)

-- | Parse a Let expression
pLet :: Parser TT (Exp TT)
pLet = PLet <$> pAtom [Reserved Let]
   <*> ((pBlockOf' (Block <$> pBlocks (pTr el [(Reserved In),(ReservedOp Pipe),(ReservedOp Equal)])))
        <|> (pEmptyBL <* pTestTok pEol))
   <*>  pOpt (PAtom <$> exact [Reserved In] <*> pure [])
    where pEol = [endBlock]
          el = [(Reserved Data),(Reserved Type)]

-- | Parse a class decl
pClass :: Parser TT (Exp TT)
pClass = PClass <$> pAtom [Reserved Class]
     <*> (TC <$> pOpt (Bin <$> (pSContext <|> (pParen (pSepBy pSContext pComma) pComments))
                       <*> ppAtom [ReservedOp DoubleRightArrow]))
     <*> ppAtom [ConsIdent]
     <*> ppAtom [VarIdent]
     <*> (((pMany pErr') <* pTestTok pEol) <|> pW)
        where pW = Bin <$> pAtom [Reserved Where]
               <*> please (pBlockOf $ pTree pWBlock err' atom')
              pEol = [nextLine, endBlock]
              pErr' = PError
                  <$> recoverWith (sym $ not . (\x -> isComment x
                                               || elem x [CppDirective
                                                         , endBlock
                                                         , nextLine]))
                  <*> pure (newT '!')
                  <*> pComments
              err' = [(Reserved In)]
              atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

pSContext :: Parser TT (Exp TT)
pSContext = Bin <$> pAtom [ConsIdent] <*> ppAtom [VarIdent]

-- | Parse instances, no extensions are supported, but maybe multi-parameter should be supported
pInstance :: Parser TT (Exp TT)
pInstance = PInstance <$> pAtom [Reserved Instance]
        <*> (TC <$> pOpt (Bin <$> (pSContext <|> (pParen (pSepBy pSContext pComma) pComments))
                       <*> ppAtom [ReservedOp DoubleRightArrow]))
        <*> ppAtom [ConsIdent]
        <*> pInst
        <*> (Bin <$> (pMany pErr <* pTestTok pEol) <*> pW)
        where pW = Bin <$> ppAtom [Reserved Where]
               <*> (pBlockOf $ pTree pWBlock err' atom')
              pInst = please (pAtom [ConsIdent] 
                              <|> pParen (many (pTree' [(Reserved Data), (Reserved Type)] [])) pComments
                              <|> pBrack (many (pTree' [(Reserved Data), (Reserved Type)] [])) pComments)
              pEol = [nextLine, startBlock, endBlock, (Reserved Where)]
              err' = [(Reserved In)]
              atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

-- check if pEq can be used here instead problem with optional ->
pGuard :: Parser TT (Exp TT)
pGuard = PGuard
     <$> some (PGuard' <$> (exact [ReservedOp Pipe]) <*>
               -- comments are by default parsed after this
               (Expr <$> (pTr' err at))
               <*> please (PAtom <$> exact
                            [(ReservedOp Equal),(ReservedOp RightArrow)]
                            <*> pure [])
               -- comments are by default parsed after this -- this must be -> if used in case
               <*> (Expr <$> pTr' err' at'))
  where err  = [(Reserved Class),(Reserved Instance),(Reserved Data), (Reserved Type)]
        at   = [(ReservedOp RightArrow),(ReservedOp Equal), (ReservedOp Pipe)]
        err' = [(Reserved Class),(Reserved Instance),(Reserved In),(Reserved Data), (Reserved Type)]
        at'  = [(Reserved In), (ReservedOp Pipe)]

pRHS :: [Token] -> [Token] ->Parser TT (Exp TT)
pRHS err at = pGuard
          <|> pEq err at

pEq :: [Token] -> [Token] -> Parser TT (Exp TT)
pEq _ at = RHS <$> (PAtom <$> exact [ReservedOp Equal] <*> pure [])
       <*> (pTr' err ([(ReservedOp Equal), (ReservedOp Pipe)] `union` at))
  where err  = [ (Reserved In)
               , (ReservedOp Equal)
               , (Reserved Class)
               , (Reserved Instance)
               , (Reserved Data)
               , (Reserved Type)]

-- | Parse many of something
pMany :: Parser TT (Exp TT) -> Parser TT (Exp TT)
pMany = (<$>) Expr . many

pDTree :: Parser TT [(Exp TT)]
pDTree = pTree (\_ _ -> pure []) err atom
    where err  = [(Reserved In)]
          atom = [(ReservedOp Equal), (ReservedOp Pipe), (Reserved In)]

-- | Parse a some of something separated by the token (Special '.')
pBlocks :: Parser TT r -> Parser TT (BL.BList r)
pBlocks p =  p `BL.sepBy1` exact [nextLine]

-- | Parse a block of some something separated by the tok (Special '.')
pBlockOf :: Parser TT [(Exp TT)] -> Parser TT (Exp TT)
pBlockOf p  = Block <$> (pBlockOf' $ pBlocks p) -- see HACK above

-- | Parse something surrounded by (Special '<') and (Special '>')
pBlockOf' :: Parser TT a -> Parser TT a
pBlockOf' p = exact [startBlock] *> p <* exact [endBlock] -- see HACK above

-- | Parse something that can contain a data, type declaration or a class
pTree :: ([Token] ->[Token] -> Parser TT [(Exp TT)]) -> [Token] -> [Token] -> Parser TT [(Exp TT)]
pTree opt err at = ((:) <$> beginLine
                    <*> (pTypeSig
                         <|> (pTr err (at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))
                         <|> ((:) <$> pAtom [Special ','] <*> pTree (\_ _ -> pure []) err at))) -- change to opt err at <|> beginLine dont forget to include type data etc in err
     <|> ((:) <$> pSType <*> pure [])
     <|> ((:) <$> pSData <*> pure [])
     <|> ((:) <$> pClass <*> pure [])
     <|> ((:) <$> pInstance <*> pure [])
     <|> opt err at
    where beginLine = (pParen (pTr err at)  $ pure [])
                  <|> (PAtom <$> sym (flip notElem $ isNoise errors) <*> pure [])
                  <|> (PError <$> recoverWith
                       (sym $ flip elem $ isNoiseErr errors) 
                       <*> pure (newT '!') <*> pure [])
          errors = [ (Reserved Class)
                   , (Reserved Instance)
                   , (ReservedOp Pipe)
                   , (ReservedOp Equal)
                   , (Reserved Let)
                   , (Reserved In)
                   , (Reserved Where)
                   , (Special '{')
                   , (Special '[')]

-- | The pWBlock describes what extra things are allowed in a where clause
pWBlock :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pWBlock err at = pure []
     <|> ((:) <$> (pBrack (pTr' err (at \\ [(Special ','), (ReservedOp Pipe),(ReservedOp Equal)])) (pure []))
          <*> (pTr err $ at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))
     <|> ((:) <$> (pBrace ((pTr' err (at \\ [(Special ','),(ReservedOp Pipe),(ReservedOp Equal)]))) (pure []))
          <*> (pTr err $ at `union` [(Special ','), (ReservedOp (OtherOp "::"))]))


-- | Parse something not containing a Type, Data declaration or a class kw but parse a where
pTr :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pTr err at
    = pure []
  <|> ((:) <$> (pTree' (noiseErr \\ [(ReservedOp Pipe),(ReservedOp Equal)]) at
                <|> pBlockOf (pTr err (at \\ [(Special ',')])))
       <*> pTr err (at \\ [(ReservedOp (OtherOp "::")),(Special ','),(ReservedOp RightArrow)]))
  <|> ((:) <$> pRHS err (at \\ [(Special ','),(ReservedOp (OtherOp "::"))]) <*> pure []) -- guard or equal
  <|> ((:) <$> (PWhere <$> pAtom [Reserved Where] <*> please (pBlockOf $ pTree pWBlock err' atom'))
       <*> pTree (\_ _ -> pure []) err' atom')
    where err' = [(Reserved In)]
          atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

-- | Parse something where guards are not allowed
pTr' :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pTr' err at = pure []
          <|> ((:) <$> (pTree' ([ReservedOp Pipe] `union` err) at
                        <|> (pBlockOf (pTr err (([(ReservedOp Equal), (ReservedOp Pipe)] `union` at)
                                                \\ [(ReservedOp (OtherOp "::")),(ReservedOp RightArrow)]))))
               <*> pTr' err at)
          <|> ((:) <$> (PWhere <$> pAtom [Reserved Where] <*> please (pBlockOf $ pTree pWBlock err' atom'))
              <*> pTr' err at)
    where err' = [(Reserved In)]
          atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

-- | Parse a Tree' of expressions
pTree' ::[Token] -> [Token] -> Parser TT (Exp TT)
pTree' err at
    = (pParen ((pTr err (at \\ [Special ',']))) $ pure [])
  <|> (pBrack ((pTr' err (at \\ [(Special ','), (ReservedOp Pipe),(ReservedOp Equal)]))) $ pure [])
  <|> (pBrace ((pTr' err (at \\ [(Special ','),(ReservedOp Pipe),(ReservedOp Equal)]))) $ pure [])
  <|> pLet
  <|> (PError <$> recoverWith
       (sym $ flip elem $ (isNoiseErr err)) <*>  pure (newT '!') <*> pure [])
  <|> (PAtom <$> sym (flip notElem $ (isNoise at)) <*> pure [])
      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.

-- | Parse a typesignature 
pTypeSig :: Parser TT [(Exp TT)]
pTypeSig = ((:) <$> (TS <$>  exact [ReservedOp (OtherOp "::")]
                     <*> (pTr noiseErr []) <* pTestTok pEol) <*> pure [])
    where pEol = [(Special ';'), startBlock, endBlock, nextLine, (Special ')')]

-- | A list of keywords that usually should be an error
noiseErr :: [Token]
noiseErr = [(Reserved Class)
           , (Reserved Instance)
           , (ReservedOp Pipe)
           , (Reserved In)
           , (Reserved Data)
           , (Reserved Type)]

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
      , (Reserved Instance)
      , (Reserved Module)
      , (Reserved Import)
      , (Reserved Type)
      , (Reserved Data)
      , (Reserved Where)] ++ (fmap Special "()[]{}<>.") ++ r

-- | Parse parenthesis, brackets and braces containing
-- an expression followed by possible comments
pParen, pBrace, pBrack
       :: Parser TT [Exp TT] -> Parser TT [TT] -> Parser TT (Exp TT)

pParen p c = Paren <$> pCAtom [Special '('] c 
        <*> p <*> please (pCAtom [Special ')'] c)

pBrace p c = Paren  <$> pCAtom [Special '{'] c
         <*> p <*> please (pCAtom [Special '}'] c)

pBrack p c = Paren  <$>  pCAtom [Special '['] c
         <*> p <*> please (pCAtom [Special ']'] c)
{-# LANGUAGE FlexibleInstances, TypeFamilies
  , TemplateHaskell, DeriveDataTypeable #-}
-- Copyright (c) JP Bernardy 2008
-- Note if the layout of the first line (not comments)
-- is wrong the parser will only parse what is in the blocks given by Layout.hs
module Yi.Syntax.Haskell ( PModule (..)
                         , PModuleDecl (..)
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

-- HACK: We insert the Special '<', '>', '.',
-- which do not occur in normal haskell
-- parsing.

-- | Check if a token is a brace, this function is used to
-- fix the layout so that do { works correctly
isBrace :: TT -> Bool
isBrace (Tok b _ _) = (Special '{') == b

-- | Theese are the tokens ignored by the layout handler.
ignoredToken :: TT -> Bool
ignoredToken (Tok t _ (Posn _ _ _)) = isComment t || t == CppDirective

type Tree t = PModule t
type PAtom t = Exp t
type Block t = Exp t
type PGuard t = Exp t

-- | A program is some comments followed by a module and a body
data PModule t
    = PModule [t] (Maybe (PModule t)) -- ^ A PModule can be just comments
    | ProgMod (PModuleDecl t) (PModule t) -- ^ The module declaration part
    | Body [PImport t] (Block t) (Block t) -- ^ The body of the module
  deriving Show

-- | A module
data PModuleDecl t = PModuleDecl (PAtom t) (PAtom t) (Exp t) (Exp t)
    deriving Show

-- | Imported things
data PImport t = PImport (PAtom t) (Exp t) (PAtom t) (Exp t) (Exp t)
    deriving Show

-- | Exp can be expression or declaration
data Exp t
      -- Top declarations
    = TS t [Exp t] -- ^ Type signature 
    | PType (PAtom t) (Exp t) (Exp t) (PAtom t) (Exp t) -- ^ Type declaration
    | PData (PAtom t) (Exp t) (Exp t) (Exp t) -- ^ Data declaration
    | PData' (PAtom t) (Exp t) (Exp t) -- ^ Data declaration RHS
    | PClass (PAtom t) (Exp t) (Exp t) (Exp t) (Exp t) -- ^ Class declaration
    | PInstance (PAtom t) (Exp t) (Exp t) (Exp t) (Exp t) -- ^ Instance
      -- declaration
      -- declarations and parts of them follow
    | Paren (PAtom t) [Exp t] (PAtom t) -- ^ A parenthesized, bracked or braced
    | Block (BL.BList [Exp t]) -- ^ A block of things separated by layout
    | PAtom t [t] -- ^ An atom is a token followed by many comments
    | Expr [Exp t] -- ^
    | PWhere (PAtom t) (Exp t) -- ^ Where clause
    | Bin (Exp t) (Exp t)
       -- an error with comments following so we never color comments in wrong
       -- color. The error has an extra token, the Special '!' token to
       -- indicate that it contains an error
    | PError t t [t] -- ^ An wrapper for errors
      -- rhs that begins with Equal
    | RHS (PAtom t) [Exp t] -- ^ Righthandside of functions with =
    | Opt (Maybe (Exp t)) -- ^ An optional
    | Modid t [t] -- ^ Module identifier
    | Context (Exp t) (Exp t) (PAtom t) -- ^
    | PGuard [PGuard t] -- ^ Righthandside of functions with |
      -- the PAtom in PGuard' does not contain any comments
    | PGuard' (PAtom t) [Exp t] (PAtom t) [Exp t]
      -- type constructor is just a wrapper to indicate which highlightning to
      -- use.
    | TC (Exp t) -- ^ Type constructor
      -- data constructor same as with the TC constructor
    | DC (Exp t) -- ^ Data constructor
    | PLet (PAtom t) (Exp t) (Exp t) -- ^ let expression
    | PIn t [Exp t]
  deriving Show

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
              work (PGuard' t e t' e') = work t
                                      <> foldMap work e
                                      <> work t'
                                      <> foldMap work e'
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
                                                 <> work exp <> work exp'
                                                 <> work e''
              fold' = foldMapToksAfter begin f
    foldMapToks f = foldMap (foldMapToks f)

instance SubTree (PModule TT) where
    type Element (PModule TT) = TT
    foldMapToksAfter begin f t0 = work t0
        where work (PModule m (Just p)) = foldMapToksAfter begin f m <> work p
              work (PModule m Nothing) = foldMapToksAfter begin f m
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
$(derive makeFoldable ''PModuleDecl)
$(derive makeFoldable ''PModule)

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

getExprs :: PModule TT -> [Exp TT]
getExprs (ProgMod _ b)     = getExprs b
getExprs (Body _ exp exp') = [exp, exp']
getExprs (PModule _ (Just e)) = getExprs e
getExprs _                 = [] -- error "no match"

-- | The parser
parse :: P TT (Tree TT)
parse = pModule <* eof

-- | @pModule@ parse a module
pModule :: Parser TT (PModule TT)
pModule = PModule <$> pComments <*> optional
           (pBlockOf' (ProgMod <$> pModuleDecl
                       <*> pModBody <|> pBody))

-- | Parse a body that follows a module
pModBody :: Parser TT (PModule TT)
pModBody = ((exact [startBlock]) *>
            (Body <$> pImports
             <*> ((pTestTok elems *> pBod)
                  <|> pEmptyBL) <* (exact [Special '>'])
             <*> pBod
            <|> Body <$> noImports
             <*> ((pBod <|> pEmptyBL) <* (exact [Special '>']))
             <*> pBod))
       <|> ((exact [nextLine]) *> pBody)
       <|> Body <$> pEmpty <*> pEmptyBL <*> pEmptyBL
    where pBod  = Block <$> pBlocks pDTree
          elems = [(Special ';'), nextLine, startBlock]

-- | @pEmptyBL@ A parser returning an empty block
pEmptyBL :: Parser TT (Exp TT)
pEmptyBL = Block <$> pure BL.nil

-- | Parse a body of a program
pBody :: Parser TT (PModule TT)
pBody = Body <$> noImports <*> (Block <$> pBlocks pDTree) <*> pEmptyBL
    <|> Body <$> pImports <*> ((pTestTok elems *> (Block <$> pBlocks pDTree))
                               <|> pEmptyBL) <*> pEmptyBL
    where elems = [nextLine, startBlock]

noImports :: Parser TT [a]
noImports = (notNext [Reserved Import]) *> pEmpty
    where notNext f = testNext (\r -> (not $ isJust r)
                   || (not . elem ((tokT . fromJust) r)) f)

-- Helper functions for parsing follows
-- | Parse Variables
pVarId :: Parser TT (Exp TT)
pVarId = pAtom [VarIdent, (Reserved Other), (Reserved As)]

-- | Parse VarIdent and ConsIdent
pQvarid :: Parser TT (Exp TT)
pQvarid = pAtom [VarIdent, ConsIdent, (Reserved Other), (Reserved As)]

-- | Parse an operator using please
pQvarsym :: Parser TT (Exp TT)
pQvarsym = pParen ((:) <$> (please $ PAtom <$> sym isOperator <*> pComments)
                   <*> pEmpty)

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

pEmpty :: ((Functor f),(Applicative f)) =>  f [a]
pEmpty = pure []

pToList :: ((Functor f),(Applicative f)) =>  f a -> f [a]
pToList arg = (:) <$> arg <*> pEmpty

-- | @sym f@ returns a parser parsing @f@ as a special symbol
sym :: (Token -> Bool) -> Parser TT TT
sym f = symbol (f . tokT)

-- | @exact tokList@ parse anything that is in @tokList@
exact :: [Token] -> Parser TT TT
exact = sym . (flip elem)

-- | Create a special character symbol
newT :: Char -> TT
newT = tokFromT . Special

-- | @please p@ returns a parser parsing either @p@ or recovers with the
-- (Special '!') token.
please :: Parser TT (Exp TT) -> Parser TT (Exp TT)
please = (<|>) (PError <$> (recoverWith $ pure $ newT '!')
                <*> pure (newT '!')
                <*> pEmpty)

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

ppAtom at =  pAtom at <|> recoverAtom

recoverAtom :: Parser TT (Exp TT)
recoverAtom = PAtom <$> (recoverWith $ pure $ newT '!') <*> pEmpty

-- | Parse an atom with optional comments
pCAtom :: [Token] -> Parser TT [TT] -> Parser TT (Exp TT)
pCAtom r c = PAtom <$> exact r <*> c

-- | @pSepBy p sep@ parse /zero/ or more occurences of @p@, separated
-- by @sep@, with optional ending @sep@,
-- this is quite similar to the sepBy function provided in
-- Parsec, but this one allows an optional extra separator at the end.
--
-- > commaSep p = p `pSepBy` (symbol (==(Special ',')))

pSepBy :: Parser TT (Exp TT) -> Parser TT (Exp TT) -> Parser TT [Exp TT]
pSepBy p sep = pEmpty
           <|> (:) <$> p <*> (pSepBy1 p sep <|> pEmpty)
           <|> pToList sep -- optional ending separator
    where pSepBy1 r p' = (:) <$> p' <*> (pEmpty <|> pSepBy1 p' r)

-- | Separate a list of things separated with comma inside of parenthesis
pParenSep :: Parser TT (Exp TT) -> Parser TT (Exp TT)
pParenSep = pParen . (flip pSepBy pComma)

-- | Parse a comma separator
pComma :: Parser TT (Exp TT)
pComma = pAtom [Special ',']

-- End of helper functions Parsing different parts follows

-- | Parse a Module declaration
pModuleDecl :: Parser TT (PModuleDecl TT)
pModuleDecl = PModuleDecl <$> pAtom [Reserved Module]
          <*> ppAtom [ConsIdent]
          <*> pOpt (pParenSep pExport)
          <*> ((optional $ exact [nextLine]) *>
               (Bin <$> ppAtom [Reserved Where])
               <*> pMany pErr') <* pTestTok elems
    where elems = [nextLine, startBlock, endBlock]
          pErr' = PError <$>
                  recoverWith (sym $ not .
                               (\x -> isComment x
                                || elem x [CppDirective
                                          , startBlock
                                          , endBlock
                                          , nextLine]))
              <*> pure (newT '!')
              <*> pComments

pExport :: Parser TT (Exp TT)
pExport = (optional $ exact [nextLine]) *> please
        ( pVarId
          <|> pEModule
          <|> Bin <$> pQvarsym <*> (DC <$> pOpt expSpec) -- typeOperator
          <|> Bin <$> (TC <$> pQtycon) <*> (DC <$> pOpt expSpec)
        )
        where pDotOp = (ReservedOp $ OtherOp "..")
              expSpec = pParen ((pToList $ please (pAtom [pDotOp]))
                                <|> pSepBy pQvarid pComma)

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
        where pImpSpec = Bin <$> (pKW [Reserved Hiding] $
                                  please pImpS) <*> pMany pErr
                     <|> Bin <$> pImpS <*> pMany pErr
                     <|> pMany pErr
              pImpS    = DC <$> pParenSep pExp'
              pExp'    = Bin
                     <$> (PAtom <$> sym
                          (\x -> (flip elem [VarIdent, ConsIdent] x)
                           || isOperator x) <*> pComments
                          <|>  pQvarsym) <*> pOpt pImpS

-- | Parse simple types
pType :: Parser TT (Exp TT)
pType = PType <$> pAtom [Reserved Type]
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
pSimpleType = Bin <$> (TC <$> ppCons) <*> pMany pQvarid
          <|> pParen ((:) <$> (TC <$> ppCons) <*> many pQvarid)

-- | Parse data declarations
pData :: Parser TT (Exp TT)
pData = PData <$> pAtom [(Reserved Data)]
     <*> pOpt (TC <$> pContext)
     <*> (Bin <$> (TC <$> pSimpleType)   <*> pMany pErr')
     <*> (pOpt (Bin <$> pDataRHS <*> pMany pErr)) <* pTestTok pEol
    where pErr' = PError
              <$> recoverWith (sym $ not .
                               (\x -> isComment x
                                || elem x [ CppDirective
                                          , (ReservedOp Equal)
                                          , (Reserved Deriving)]
                               ))
              <*> pure (newT '!')
              <*> pComments
          pEol = [(Special ';'), nextLine, endBlock]

-- | Parse second half of the data declaration, if there is one
pDataRHS :: Parser TT (Exp TT)
pDataRHS = PData' <$> pAtom eqW -- either we have standard data
                                -- or we have GADT:s
      <*> (please pConstrs
           <|> pBlockOf' (Block <$> many pGadt
                          `BL.sepBy1` exact [nextLine]))
      <*> pOpt pDeriving
      <|> pDeriving
    where eqW = [(ReservedOp Equal),(Reserved Where)]

-- | Parse an GADT declaration
pGadt :: Parser TT (Exp TT)
pGadt = Bin <$> (DC <$> pQtycon)
    <*> (ppOP [ReservedOp $ OtherOp "::"]
         (Bin <$> pOpt pContext <*>
          (pTypeRhs <|> (pOP [Operator "!"] pAtype) <|> pErr)))
    <|>  pErr

-- | Parse a deriving
pDeriving :: Parser TT (Exp TT)
pDeriving = TC
        <$> (pKW [Reserved Deriving]
             (please (pParen
              (((:) <$> please pQtycon
                <*> many (Bin <$> pComma <*> please pQtycon)))
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
      <|> (pParen $ many $ pTree err [])
      <|> (pBrack $ many $ pTree err [])
        where err = [(Reserved Data), (Reserved Type)]

pContext :: Parser TT (Exp TT)
pContext = Context <$> pOpt pForAll
       <*> (TC <$> (pClass' <|> pParenSep pClass'))
       <*> ppAtom [ReservedOp DoubleRightArrow]
        where pClass' :: Parser TT (Exp TT)
              pClass' = Bin <$> pQtycon
                   <*> (please pVarId
                        <|> pParen ((:) <$> please pVarId
                                    <*> many pAtype'))

-- | Parse for all
pForAll :: Parser TT (Exp TT)
pForAll = pKW [Reserved Forall]
          (Bin <$> pVars <*> ppAtom [Operator "."])

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
          st = pBrace (pToList $ pOpt
                       $ Bin <$> pFielddecl
                       <*> pMany (Bin <$> pComma <*> pFielddecl))

-- | Parse optional strict variables
strictF :: Parser TT (Exp TT) -> Parser TT (Exp TT)
strictF a = Bin <$> pOpt (pAtom [Operator "!"]) <*> a

pFielddecl ::Parser TT (Exp TT)
pFielddecl = Bin <$> pVars
         <*> pOpt (pOP [ReservedOp $ OtherOp "::"]
                   (pTypeRhs
                    <|> pKW [Operator "!"] pAtype
                    <|> pErr))

-- | Exporting module
pEModule ::Parser TT (Exp TT)
pEModule = pKW [Reserved Module]
         $ please (Modid <$> exact [ConsIdent] <*> pComments)

-- | Parse a Let expression
pLet :: Parser TT (Exp TT)
pLet = PLet <$> pAtom [Reserved Let]
   <*> (pBlockOf'
        (Block <$> pBlocks 
         (pTr el [(ReservedOp Pipe),(ReservedOp Equal)]))
        <|> (pEmptyBL <* pTestTok pEol))
   <*>  pOpt (pCAtom [Reserved In] pEmpty)
    where pEol = [endBlock]
          el = [(Reserved Data),(Reserved Type)]

-- | Parse a class decl
pClass :: Parser TT (Exp TT)
pClass = PClass <$> pAtom [Reserved Class]
     <*> (TC <$> pOpt 
          (Bin <$> (pSContext <|> pParenSep pSContext)
           <*> ppAtom [ReservedOp DoubleRightArrow]))
     <*> ppAtom [ConsIdent]
     <*> ppAtom [VarIdent]
     <*> (Bin <$> ((pMany pErr) <* pTestTok pEol) <*> pOpt pW)
        where pW = PWhere <$> pAtom [Reserved Where]
               <*> (Bin <$> please (pBlockOf $ pWBlock [] atom')
                    <*> (Expr <$> pWBlock [] atom'))
              pEol = [nextLine, endBlock, startBlock, (Reserved Where)]
              atom' = [(ReservedOp Equal),(ReservedOp Pipe), (Reserved In)]

pSContext :: Parser TT (Exp TT)
pSContext = Bin <$> pAtom [ConsIdent] <*> ppAtom [VarIdent]

-- | Parse instances, no extensions are supported, but maybe
-- multi-parameter should be supported
pInstance :: Parser TT (Exp TT)
pInstance = PInstance <$> pAtom [Reserved Instance]
        <*> (TC <$> pOpt (Bin <$> (pSContext <|> pParenSep pSContext)
              <*> ppAtom [ReservedOp DoubleRightArrow]))
        <*> ppAtom [ConsIdent]
        <*> pInst
        <*> (Bin <$> (pMany pErr <* pTestTok pEol) <*> please pW)
        where pW = PWhere <$> pAtom [Reserved Where]
               <*> (Bin <$> please (pBlockOf $ pWBlock [] atom')
                    <*> (Expr <$> pWBlock [] atom'))
              pInst = please
                    ( pAtom [ConsIdent]
                      <|> pParen (many (pTree [(Reserved Data)
                                               , (Reserved Type)]
                                        []))
                      <|> pBrack (many (pTree [(Reserved Data)
                                               , (Reserved Type)] 
                                        [])))
              pEol = [nextLine, startBlock, endBlock, (Reserved Where)]
--               err' = [(Reserved In)]
              atom' = [(ReservedOp Equal),(ReservedOp Pipe)]

-- check if pEq can be used here instead problem with optional ->
pGuard :: Parser TT (Exp TT)
pGuard = PGuard
     <$> some (PGuard' <$> (pCAtom [ReservedOp Pipe] pEmpty) <*>
               -- comments are by default parsed after this
               (pTr' err at)
               <*> please (pCAtom [(ReservedOp Equal),(ReservedOp RightArrow)]
                           pEmpty)
               -- comments are by default parsed after this
               -- this must be -> if used in case
               <*> pTr' err' at')
  where err  = [(Reserved Class), (Reserved Instance)
               , (Reserved Data), (Reserved Type)]
        at   = [(ReservedOp RightArrow),(ReservedOp Equal)
               , (ReservedOp Pipe)]
        err' = [(Reserved Class),(Reserved Instance)
               ,(Reserved Data), (Reserved Type)]
        at'  = [(ReservedOp Pipe)]

pFunRHS :: [Token] -> [Token] -> Parser TT (Exp TT)
pFunRHS err at = Bin <$> (pGuard
                          <|> pEq err at) <*> pOpt pst
    where pst = Expr <$> ((:) <$> (PWhere <$> pAtom [Reserved Where]
                                   <*> please (pBlockOf $ pFunLHS err at))
                          <*> pTr' err at)

pFunLHS :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pFunLHS err at = (:) <$> beginLine
             <*> (pTypeSig
                  <|> (pTr err (at `union` [(Special ',')
                                           , (ReservedOp (OtherOp "::"))]))
                  <|> ((:) <$> pAtom [Special ',']
                       <*> (pFunLHS err at <|> pEmpty)))
        where beginLine = pCParen (pTr err at) pEmpty
                      <|> pCBrack (pTr err at) pEmpty
                      <|> (PAtom <$> sym (flip notElem $ isNoise errors)
                           <*> pEmpty)
                      <|> (PError <$> recoverWith
                           (sym $ flip elem $ isNoiseErr errors)
                           <*> pure (newT '!') <*> pEmpty)
              errors = [ (Reserved Class)
                       , (Reserved Instance)
                       , (ReservedOp Pipe)
                       , (ReservedOp Equal)
                       , (Reserved Let)
                       , (Reserved Where)
                       , (Special '{')]

pEq :: [Token] -> [Token] -> Parser TT (Exp TT)
pEq _ at = RHS <$> (pCAtom [ReservedOp Equal] pEmpty)
       <*> (pTr' err ([(ReservedOp Equal), (ReservedOp Pipe)] `union` at))
  where err  = [ (ReservedOp Equal)
               , (Reserved Class)
               , (Reserved Instance)
               , (Reserved Data)
               , (Reserved Type)]

-- | Parse many of something
pMany :: Parser TT (Exp TT) -> Parser TT (Exp TT)
pMany = (<$>) Expr . many

pDTree :: Parser TT [(Exp TT)]
pDTree = pTopDecl [] atom
    where atom = [(ReservedOp Equal), (ReservedOp Pipe)]

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
pTopDecl :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pTopDecl err at = pFunLHS err at
                  <|> pToList pType
                  <|> pToList pData
                  <|> pToList pClass
                  <|> pToList pInstance
                  <|> pEmpty

-- | The pWBlock describes what extra things are allowed in a where clause
pWBlock :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pWBlock err at = pTopDecl err at
     <|> ((:) <$> (pCBrack (pTr' err (at \\ [(Special ',')
                                           , (ReservedOp Pipe)
                                           , (ReservedOp Equal)])) pEmpty)
          <*> (pTr err $ at `union` [(Special ',')
                                    , (ReservedOp (OtherOp "::"))]))
     <|> ((:) <$> (pCBrace ((pTr' err (at \\ [(Special ','),(ReservedOp Pipe)
                                            , (ReservedOp Equal)]))) pEmpty)
          <*> (pTr err $ at `union` [(Special ',')
                                    , (ReservedOp (OtherOp "::"))]))

-- | Parse something not containing a Type, Data declaration or a class kw
--  but parse a where
pTr :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pTr err at
    = pEmpty
  <|> ((:) <$> (pTree ((noiseErr `union` [Reserved Where])
                       \\ [(ReservedOp Pipe),(ReservedOp Equal)]) at
                <|> pBlockOf (pTr err (at \\ [(Special ',')])))
       <*> pTr err (at \\ [(ReservedOp (OtherOp "::")), (Special ',')
                          , (ReservedOp RightArrow)]))
  <|> pToList (pFunRHS err (at \\ [(Special ','),(ReservedOp (OtherOp "::"))]))

-- | Parse something where guards are not allowed
pTr' :: [Token] -> [Token] -> Parser TT [(Exp TT)]
pTr' err at = pEmpty
          <|> (:) <$> (pTree ([(ReservedOp Pipe)
                              , (Reserved Where)
                              , (ReservedOp Equal)] `union` err) at
                       <|> (pBlockOf (pTr err ((atom' `union` at)
                                               \\ [(ReservedOp (OtherOp "::"))
                                                  , (ReservedOp RightArrow)]
                                              ))))
              <*> pTr' err at
    where atom' = [(ReservedOp Equal),(ReservedOp Pipe)]

-- | Parse a Tree of expressions
pTree ::[Token] -> [Token] -> Parser TT (Exp TT)
pTree err at
    = pCParen ((pTr err (at \\ [Special ',']))) pEmpty
  <|> pCBrack ((pTr' err (at \\ notAtom))) pEmpty
  <|> pCBrace ((pTr' err (at \\ notAtom))) pEmpty
  <|> pLet
  <|> (PError <$> recoverWith
       (sym $ flip elem $ (isNoiseErr err)) <*>  pure (newT '!') <*> pEmpty)
  <|> (PAtom <$> sym (flip notElem (isNoise at)) <*> pEmpty)
      -- note that, by construction, '<' and '>' will always be matched, so
      -- we don't try to recover errors with them.
        where notAtom = [(Special ','), (ReservedOp Pipe), (ReservedOp Equal)]

-- | Parse a typesignature 
pTypeSig :: Parser TT [(Exp TT)]
pTypeSig = pToList (TS <$>  exact [ReservedOp (OtherOp "::")]
                     <*> (pTr noiseErr []) <* pTestTok pEol)
    where pEol = [startBlock, endBlock, nextLine]-- , (Special ')')]

-- | A list of keywords that usually should be an error
noiseErr :: [Token]
noiseErr = [(Reserved Class)
           , (Reserved Instance)
           , (ReservedOp Pipe)
           , (Reserved Data)
           , (Reserved Type)]

-- | List of things that allways should be parsed as errors
isNoiseErr :: [Token] -> [Token]
isNoiseErr r
    = [ (Reserved Module)
      , (Reserved Import)
      , (Reserved In)
      , (Reserved Type)
      , (Reserved Data)
      , (Special '}')
      , (Special ')')
      , (Special ']')] ++ r

-- | List of things that never should be parsed as an atom
isNoise :: [Token] -> [Token]
isNoise r
    = [ (Reserved Let)
      , (Reserved In)
      , (Reserved Class)
      , (Reserved Instance)
      , (Reserved Module)
      , (Reserved Import)
      , (Reserved Type)
      , (Reserved Data)
      , (Reserved Where)] ++ (fmap Special "()[]{}<>.") ++ r

-- | Parse parenthesis, brackets and braces containing
-- an expression followed by possible comments
pCParen, pCBrace, pCBrack
       :: Parser TT [Exp TT] -> Parser TT [TT] -> Parser TT (Exp TT)

pCParen p c = Paren <$> pCAtom [Special '('] c
          <*> p <*> (recoverAtom <|> pCAtom [Special ')'] c)

pCBrace p c = Paren  <$> pCAtom [Special '{'] c
          <*> p <*> (recoverAtom <|> pCAtom [Special '}'] c)

pCBrack p c = Paren  <$>  pCAtom [Special '['] c
          <*> p <*> (recoverAtom <|> pCAtom [Special ']'] c)

pParen = flip pCParen pComments

pBrace = flip pCBrace pComments

pBrack = flip pCBrack pComments


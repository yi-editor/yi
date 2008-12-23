{-# LANGUAGE CPP #-}
-- ----------------------------------------------------------------------------
-- |
-- Module	: ExprSearch
-- Author	: Simon Marlow <simonmar@microsoft.com>
-- Copyright    : (c) Microsoft Corporation, All Rights Reserved
-- 
-- Searching for names in abstract syntax
--
-- ----------------------------------------------------------------------------

module Shim.ExprSearch ( 
        findExprInCheckedModule, FindResult(..)
   ) where

import GHC
import Id
import HsSyn
import Module
import Bag
import SrcLoc
import DataCon	( dataConWrapId )

import Foreign
import FiniteMap
infix 1 `implies`

-- -----------------------------------------------------------------------------
-- Expression searching

-- Here we search the abstract syntax tree for the minimal expression
-- enclosing the selected span.  For now, we only consider a point
-- span, so the minimal expression will always be a variable or
-- literal (keywords etc. are ignored - we just return NotFound).

data FindResult
  = FoundId     Id
  | FoundName   Name
  | FoundLit    HsLit
  | FoundModule ModuleName
  | NotFound

#if __GLASGOW_HASKELL__ < 610
findExprInCheckedModule :: Int -> Int -> CheckedModule -> FindResult
findExprInCheckedModule line col (CheckedModule {
					parsedSource = hsSource,
					renamedSource = mb_rnSource,
					typecheckedSource = mb_tcSource }) =
#else
findExprInCheckedModule :: Int -> Int -> TypecheckedModule -> FindResult
findExprInCheckedModule line col mdl =
  let hsSource = parsedSource mdl
      mb_rnSource = renamedSource mdl
      mb_tcSource = Just $ typecheckedSource mdl
  in
#endif
  case doSearch searchLBinds FoundId mb_tcSource of
    NotFound -> case doSearch searchRenamedSource FoundName mb_rnSource of
                  NotFound -> doSearchModule hsSource
                  res      -> res
    res      -> res
  where
    doSearch f ret (Just x) = runSearch line col ret (f x)
    doSearch f ret Nothing  = NotFound
#if __GLASGOW_HASKELL__ > 606
    doSearchModule (L span (HsModule _ _ decls _ _ _ _)) =
      runSearch line col undefined (searchList searchLImportDecl decls)

searchRenamedSource (group, _, _, _, _) = searchGroup group
#else
    doSearchModule (L span (HsModule _ _ decls _ _)) =
      runSearch line col undefined (searchList searchLImportDecl decls)

searchRenamedSource (group, _, _) = searchGroup group
#endif


-- -----------------------------------------------------------------------------
-- Import declarations searching

-- -----------------------------------------------------------------------------
-- Utils used in expr searching

searchList :: (a -> Search b) -> [a] -> Search b
searchList f xs = foldr orSearch failSearch (map f xs)

searchBag :: (a -> Search b) -> Bag a -> Search b
searchBag f bag = searchList f (bagToList bag)

searchMaybe :: (a -> Search b) -> Maybe a -> Search b
searchMaybe f Nothing = failSearch
searchMaybe f (Just a) = f a

lsearch :: (a -> Search b) -> Located a -> Search b
lsearch f (L span a) = contSpan span (f a)

lsearch' :: (a -> Search b) -> Located a -> Search b
lsearch' f (L span a) = contSpan span (f a)

-- -----------------------------------------------------------------------------
-- Binds

searchLBinds binds = searchBag searchLBind binds

searchLBind (L _ (AbsBinds _ _ exports bs))
  = extendIdMap pairs $ searchLBinds bs
  where pairs = [(mono,poly) | (_tvs,poly,mono,_) <- exports]
searchLBind (L span bind)
  = contSpan span $ searchBind bind

#if __GLASGOW_HASKELL__ > 606
searchBind (FunBind (L idspan id) _ lmatches _ _ _)
#else
searchBind (FunBind (L idspan id) _ lmatches _ _)
#endif
  = checkId idspan id `orSearch`
    searchMatchGroup lmatches
searchBind (PatBind pat grhss _ _)
  = searchLPat pat `orSearch` searchGRHSs grhss
searchBind _ = failSearch

searchLocalBinds (HsValBinds vbinds)
  = searchValBinds vbinds
searchLocalBinds (HsIPBinds (IPBinds lipbinds binds))
  = searchList searchLIPBind lipbinds `orSearch` searchLBinds binds
searchLocalBinds EmptyLocalBinds
  = failSearch

searchValBinds (ValBindsIn  binds sigs)
  = searchLBinds binds `orSearch` searchList searchLSig sigs
searchValBinds (ValBindsOut binds sigs)
  = foldr (\(_,binds) cont -> searchLBinds binds `orSearch` cont) failSearch binds
   `orSearch` Search (\line col idmap ret -> runSearch line col FoundName (searchList searchLSig sigs))

searchLIPBind lipbind = lsearch searchIPBind lipbind
searchIPBind (IPBind _ipname e) = searchLExpr e

-- -----------------------------------------------------------------------------
-- Patterns

searchLPat (L span (VarPat id))  = checkId span id
searchLPat (L span (LitPat lit)) = checkLiteral span lit
#if __GLASGOW_HASKELL__ >= 610
searchLPat (L span (NPat lit _ _)) = checkLiteral span (over_lit_lit lit)
#else
searchLPat (L span (NPat lit _ _ _)) = checkLiteral span (over_lit_lit lit)
#endif
   where
     over_lit_lit :: HsOverLit id -> HsLit
#if __GLASGOW_HASKELL__ >= 610
     over_lit_lit (OverLit (HsIntegral i) _ _ _) = HsIntPrim i
     over_lit_lit (OverLit (HsFractional f) _ _ _) = HsFloatPrim f
     over_lit_lit (OverLit (HsIsString s) _ _ _) = HsStringPrim s
#else
     over_lit_lit (HsIntegral i _) = HsIntPrim   i
     over_lit_lit (HsFractional f _) = HsFloatPrim f
#endif
searchLPat lpat = lsearch searchPat lpat

searchPat pat = 
  case pat of
    LazyPat p 	    -> searchLPat p
    AsPat lid p     -> searchLId lid `orSearch` searchLPat p
    ParPat p 	    -> searchLPat p
    ListPat ps _    -> searchList searchLPat ps
    TuplePat ps _ _ -> searchList searchLPat ps
    PArrPat ps _    -> searchList searchLPat ps

    ConPatOut (L span con) _ _ _ details _ -> 
      inSpan span (Search $ \line col idmap ret -> FoundId (dataConWrapId con))
      `orSearch` searchConDetails searchLPat details

    NPlusKPat lid int _ _ -> searchLId lid
	-- ToDo: the int should be located

    TypePat t -> searchLType t

    SigPatOut p _ -> searchLPat p
	-- ToDo: the type should be located

    _other -> failSearch

searchConDetails search details
  = case details of
	PrefixCon ps  -> searchList search ps
#if __GLASGOW_HASKELL__ > 606
	RecCon (HsRecFields fields dotdot) -> searchList rec fields
		where rec (HsRecField lid p _) = searchLId lid `orSearch` search p
#else
        RecCon fields -> searchList rec fields
                where rec (lid,p) = searchLId lid `orSearch` search p
#endif
	InfixCon p1 p2 -> search p1 `orSearch` search p2

#if __GLASGOW_HASKELL__ > 606
searchConDeclDetails search (PrefixCon ps)   = searchList search ps
searchConDeclDetails search (InfixCon p1 p2) = search p1 `orSearch` search p2
searchConDeclDetails search (RecCon fields) = searchList rec fields
		where rec (ConDeclField ln ty _) = searchLId ln `orSearch` search ty
#endif

-- -----------------------------------------------------------------------------
-- Matches

searchMatchGroup (MatchGroup lmatches _) = searchLMatches lmatches

searchLMatches lmatches = searchList searchLMatch lmatches

searchLMatch lmatch@(L span _) = lsearch' searchMatch lmatch

searchMatch (Match pats result_ty grhss)
  = searchList searchLPat pats
   `orSearch` searchMaybe searchLType result_ty
   `orSearch` searchGRHSs grhss

searchGRHSs (GRHSs lgrhs bindgroups)
  = searchList searchLGRHS lgrhs
    `orSearch` searchLocalBinds bindgroups

searchLGRHS lgrhs@(L span _) = lsearch' searchGRHS lgrhs

searchGRHS (GRHS stmts expr) 
  = searchLStmts stmts
    `orSearch` searchLExpr expr

-- -----------------------------------------------------------------------------
-- Statements

searchLStmts lstmts = searchList searchLStmt lstmts
searchLStmt lstmt = lsearch searchStmt lstmt

searchStmt (BindStmt pat lexpr _ _)
  = searchLPat pat
    `orSearch` searchLExpr lexpr
searchStmt (LetStmt bindgroups)
  = searchLocalBinds bindgroups
searchStmt (ExprStmt lexpr _ _)
  = searchLExpr lexpr
searchStmt (ParStmt pars)
  = searchList (searchList searchLStmt . fst) pars
searchStmt (RecStmt lstmts _ _ exprs _)
  = searchLStmts lstmts

-- -----------------------------------------------------------------------------
-- Expressions

searchLId (L span id) = checkId span id

searchLExprs lexprs = searchList searchLExpr lexprs

searchLExpr (L span (HsLit lit)) = checkLiteral span lit
searchLExpr (L span (HsVar id))  = checkId span id
#if __GLASGOW_HASKELL__ > 606
searchLExpr (L span (HsWrap _ e)) = checkId span id
   where id = getCornerId e
#else
searchLExpr (L span (TyApp e _)) = checkId span id
   where id = getCornerIdL e
searchLExpr (L span (DictApp e _)) = checkId span id
   where id = getCornerIdL e
#endif
searchLExpr lexpr = lsearch searchExpr lexpr

-- The typechecker likes to expand identifiers with type applications
-- and dictionary applications, but it doesn't propagate the srcloc
-- info down.  So we spot those expanded expressions here:
getCornerIdL (L _ e) = getCornerId e

getCornerId (HsVar id)    = id
#if __GLASGOW_HASKELL__ > 606
getCornerId (HsWrap _ e)   = getCornerId e
#else
getCornerId (TyApp e _)   = getCornerIdL e
getCornerId (DictApp e _) = getCornerIdL e
#endif

searchExpr e 
  = case e of
      HsLam	lmatch  -> searchMatchGroup lmatch
      HsApp	e1 e2  -> searchLExpr e1 `orSearch` searchLExpr e2

      OpApp	e1 op _fix e2 -> searchLExpr e1 
				`orSearch` searchLExpr op
				`orSearch` searchLExpr e2

      NegApp	e _ -> searchLExpr e

      HsPar	e -> searchLExpr e

      SectionL	e op -> searchLExpr e  `orSearch` searchLExpr op
      SectionR  op e -> searchLExpr op `orSearch` searchLExpr e

      HsCase e lmatches -> searchLExpr e 
			   `orSearch` searchMatchGroup lmatches

      HsIf  e1 e2 e3 -> searchLExpr e1
			`orSearch` searchLExpr e2
			`orSearch` searchLExpr e3

      HsLet bindgroups e -> searchLocalBinds bindgroups
			    `orSearch` searchLExpr e

      HsDo _ lstmts e _ -> searchLStmts lstmts
                            `orSearch` searchLExpr e

      ExplicitList _ lexprs  -> searchLExprs lexprs
      ExplicitPArr _ lexprs  -> searchLExprs lexprs
      ExplicitTuple lexprs _ -> searchLExprs lexprs

      RecordCon	lid _ recbinds -> searchLId lid 
				`orSearch` searchRecBinds recbinds
#if __GLASGOW_HASKELL__ > 606
      RecordUpd	e recbinds _ _ _ -> searchLExpr e
				`orSearch` searchRecBinds recbinds
#else

      RecordUpd	e recbinds   _ _ -> searchLExpr e
				`orSearch` searchRecBinds recbinds
#endif
      ExprWithTySig e ty -> searchLExpr e `orSearch` searchLType ty

      ArithSeq e seqinfo -> searchSeqInfo seqinfo

      PArrSeq e seqinfo -> searchSeqInfo seqinfo

      HsSCC _ e -> searchLExpr e

      HsCoreAnn _ e  -> searchLExpr e
#if __GLASGOW_HASKELL__ > 606
      HsWrap _ e   -> searchExpr e
#else
      TyLam _ e   -> searchLExpr e
      TyApp e _   -> searchLExpr e
      DictLam _ e -> searchLExpr e
      DictApp e _ -> searchLExpr e
#endif
      _ -> failSearch  -- nothing else contains any names.
		    -- Implicit parameters: we can't jump to the decl, 
		    -- because they are dynamically scoped!

searchSeqInfo (From e1)
  = searchLExpr e1
searchSeqInfo (FromThen e1 e2)
  = searchLExpr e1 `orSearch` searchLExpr e2
searchSeqInfo (FromTo e1 e2)
  = searchLExpr e1 `orSearch` searchLExpr e2
searchSeqInfo (FromThenTo e1 e2 e3)
  = searchLExpr e1 `orSearch` searchLExpr e2 `orSearch` searchLExpr e3

searchRecBinds :: Ord b => HsRecordBinds b -> Search b
#if __GLASGOW_HASKELL__ > 606
searchRecBinds (HsRecFields fields dotdot) = searchList searchRecBind fields
  where searchRecBind (HsRecField (L span field) expr _) =
	   checkId span field `orSearch` searchLExpr expr
#else
searchRecBinds recbinds = searchList searchRecBind recbinds
  where searchRecBind (L span field,expr) =
	   checkId span field `orSearch` searchLExpr expr
#endif


-- ----------------------------------------------------------------------------
-- Sigs

searchLSig lsig = lsearch searchSig lsig

searchSig (TypeSig lid tp) = searchLId lid `orSearch` searchLType tp
searchSig (SpecSig lid tp _) = searchLId lid `orSearch` searchLType tp
searchSig (InlineSig lid _) = searchLId lid
searchSig (SpecInstSig tp) = searchLType tp
searchSig (FixSig fix) = searchFixitySig fix

-- ----------------------------------------------------------------------------
-- FixitySig

searchLFixitySig fix = lsearch searchFixitySig fix

searchFixitySig (FixitySig lid _) = searchLId lid

-- ----------------------------------------------------------------------------
-- Types

searchLTypes ltps = searchList searchLType ltps

searchLType lty = lsearch searchType lty

searchType (HsForAllTy _ _ ctxt tp) = searchLContext ctxt `orSearch` searchLType tp
searchType (HsTyVar id) = Search $ (\line col idmap ret -> ret id)
searchType (HsBangTy _ tp) = searchLType tp
searchType (HsAppTy tp1 tp2) = searchLType tp1 `orSearch` searchLType tp2
searchType (HsFunTy tp1 tp2) = searchLType tp1 `orSearch` searchLType tp2
searchType (HsListTy tp) = searchLType tp
searchType (HsPArrTy tp) = searchLType tp
searchType (HsTupleTy _ tps) = searchLTypes tps
searchType (HsOpTy tpl lid tpr) = searchLType tpl `orSearch` searchLId lid `orSearch` searchLType tpr
searchType (HsParTy tp) = searchLType tp
searchType (HsPredTy pred) = searchPred pred
searchType _ = failSearch

-- ----------------------------------------------------------------------------
-- Context

searchLContext lctxt = lsearch searchContext lctxt

searchContext lpreds = searchList searchLPred lpreds

-- ----------------------------------------------------------------------------
-- Pred

searchLPred (L span (HsClassP id tps)) = checkId span id `orSearch` searchLTypes tps
searchLPred lpred = lsearch searchPred lpred

searchPred (HsClassP _ tps) = searchLTypes tps
searchPred (HsIParam _ tp) = searchLType tp

-- ----------------------------------------------------------------------------
-- TyClDecl

searchLTyClDecl ltyClass = lsearch searchTyClDecl ltyClass

searchTyClDecl (ForeignType lid _ _) = searchLId lid
searchTyClDecl td@(TyData {}) =
  searchLContext (tcdCtxt td) `orSearch`
  searchLId (tcdLName td) `orSearch`
  searchList searchLConDecl (tcdCons td) `orSearch`
  searchMaybe searchLTypes (tcdDerivs td)
searchTyClDecl ts@(TySynonym {}) =
  searchLId (tcdLName ts) `orSearch`
  searchLType (tcdSynRhs ts)
searchTyClDecl cd@(ClassDecl {}) =
  searchLContext (tcdCtxt cd) `orSearch`
  searchLId (tcdLName cd) `orSearch`
  searchList searchLSig (tcdSigs cd)  `orSearch`
  searchLBinds (tcdMeths cd)
{-
searchTyClDecl (TyData _ lctxt lid _ _ constrs mb_derivs) = 
  searchLContext lctxt `orSearch`
  searchLId lid `orSearch`
  searchList searchLConDecl constrs `orSearch`
  searchMaybe searchLTypes mb_derivs
searchTyClDecl (TySynonym lid _ ltp) =
  searchLId lid `orSearch`
  searchLType ltp
searchTyClDecl (ClassDecl lctxt lid _ _ lsigs lbinds) =
  searchLContext lctxt `orSearch`
  searchLId lid `orSearch`
  searchList searchLSig lsigs `orSearch`
  searchLBinds lbinds 
-}

-- ----------------------------------------------------------------------------
-- ConDecl
  
searchLConDecl lconDecl = lsearch searchConDecl lconDecl
#if __GLASGOW_HASKELL__ > 606
searchConDecl (ConDecl lid _ lbndrs lctxt details res _) =
#else
searchConDecl (ConDecl lid _ lbndrs lctxt details res) =
#endif
  searchLId lid `orSearch`
  searchList searchLBndr lbndrs `orSearch`
  searchLContext lctxt `orSearch`
#if __GLASGOW_HASKELL__ > 606
  searchConDeclDetails searchLType details `orSearch`
#else
  searchConDetails searchLType details `orSearch`
#endif
  searchResType res
  where
    searchResType ResTyH98          = failSearch
    searchResType (ResTyGADT ltype) = searchLType ltype
    
    searchLBndr (L span (UserTyVar   id  )) = checkId span id
    searchLBndr (L span (KindedTyVar id _)) = checkId span id

-- ----------------------------------------------------------------------------
-- InstDecl

searchLInstDecl linstDecl = lsearch searchInstDecl linstDecl

#if __GLASGOW_HASKELL__ > 606
searchInstDecl (InstDecl ltp lbinds lsigs _) =
#else
searchInstDecl (InstDecl ltp lbinds lsigs) =
#endif
  searchLType ltp `orSearch`
  searchLBinds lbinds `orSearch`
  searchList searchLSig lsigs

-- ----------------------------------------------------------------------------
-- DefaultDecl

searchLDefaultDecl ldefDecl = lsearch searchDefaultDecl ldefDecl

searchDefaultDecl (DefaultDecl ltps) = searchLTypes ltps

-- ----------------------------------------------------------------------------
-- ForeignDecl

searchLForeignDecl lfDecl = lsearch searchForeignDecl lfDecl

searchForeignDecl (ForeignImport lid ltp _) = searchLId lid `orSearch` searchLType ltp
searchForeignDecl (ForeignExport lid ltp _) = searchLId lid `orSearch` searchLType ltp

-- ----------------------------------------------------------------------------
-- RuleDecl

searchLRuleDecl lruleDecl = lsearch searchRuleDecl lruleDecl

searchRuleDecl (HsRule _ _ bndrs lexpr1 _ lexpr2 _) =
  searchList searchRuleBndr bndrs `orSearch`
  searchLExpr lexpr1 `orSearch`
  searchLExpr lexpr2

searchRuleBndr (RuleBndr lid) = searchLId lid
searchRuleBndr (RuleBndrSig lid ltp) = searchLId lid `orSearch` searchLType ltp

-- ----------------------------------------------------------------------------
-- DeprecDecl

#if __GLASGOW_HASKELL__ < 610
searchLDeprecDecl (L span (Deprecation id _)) = checkId span id
#endif

-- ----------------------------------------------------------------------------
-- Group

searchGroup g@(HsGroup {}) =
  searchValBinds (hs_valds g) `orSearch`
  searchList searchLTyClDecl (hs_tyclds g) `orSearch`
  searchList searchLInstDecl (hs_instds g) `orSearch`
  searchList searchLFixitySig (hs_fixds g) `orSearch`
  searchList searchLDefaultDecl (hs_defds g) `orSearch`
  searchList searchLForeignDecl (hs_fords g) `orSearch`
#if __GLASGOW_HASKELL__ < 610
  searchList searchLDeprecDecl (hs_depds g) `orSearch`
#endif
  searchList searchLRuleDecl (hs_ruleds g)

-- ----------------------------------------------------------------------------
-- ImportDecl

searchLImportDecl ldecl = lsearch searchImportDecl ldecl

#if __GLASGOW_HASKELL__ >= 610
searchImportDecl (ImportDecl (L span modl) _ _ _ _ _) = inSpan span (Search $ \_ _ _ _ -> FoundModule modl)
#else
searchImportDecl (ImportDecl (L span modl) _ _ _ _) = inSpan span (Search $ \_ _ _ _ -> FoundModule modl)
#endif

-- ----------------------------------------------------------------------------
-- Utils

-- A search abstraction.  It's not a monad, because the main combining
-- operation is orSearch below, which doesn't have the same type as bind.
newtype Search a = Search { unSearch :: Int -> Int -> FiniteMap a a -> (a -> FindResult) -> FindResult }

failSearch = Search $ \line col idmap ret -> NotFound

orSearch :: Search a -> Search a -> Search a
Search s1 `orSearch` Search s2 = Search $ \line col idmap ret ->
  case s1 line col idmap ret of
	NotFound -> s2 line col idmap ret
	result   -> result

runSearch :: Int -> Int -> (a -> FindResult) -> Search a -> FindResult
runSearch line col ret (Search m) = m line col emptyFM ret

extendIdMap :: Ord a => [(a,a)] -> Search a -> Search a
extendIdMap pairs s = 
  Search $ \line col idmap ret -> unSearch s line col (addListToFM idmap pairs) ret

-- we accept the column after the span too: if you have the cursor at
-- the position directly after an identifier, then we'll count that as
-- part of the identifier too.  If you have two adjacent identifiers
-- (eg. f$), then you could get either.
inSpan :: SrcSpan -> Search a -> Search a
inSpan span inside
  = Search check
  where
   check line col idmap ret
     | not (isGoodSrcSpan span) = NotFound
     | line < sline = NotFound
     | line > eline = NotFound
     | (line == sline `implies` col >= scol)
       && (line == eline `implies` col <= ecol)
       = unSearch inside line col idmap ret
     | otherwise = NotFound
     where
       sloc  = srcSpanStart span
       sline = srcLocLine   sloc
       scol  = srcLocCol    sloc
       
       eloc  = srcSpanEnd   span
       eline = srcLocLine   eloc
       ecol  = srcLocCol    eloc

False `implies` b = True
True  `implies` b = b

-- For checking whether we should descend into a subexpression, check whether
-- the location we're after lies within the span of the expression.  If
-- the expression has no good src loc info, then conservatively descend anyway.
contSpan :: SrcSpan -> Search a -> Search a
contSpan span cont 
  | not (isGoodSrcSpan span) = cont
  | otherwise = inSpan span cont

-- For checking whether we've found the right token.  In this case, if the
-- token has no src loc info, we ignore it (different to contSpan)
checkLiteral :: SrcSpan -> HsLit -> Search a
checkLiteral span yes = inSpan span (Search $ \line col idmap ret -> FoundLit yes)

checkId :: Ord a => SrcSpan -> a -> Search a
checkId span id = inSpan span mapped
  where mapped = 
	  Search $ \line col idmap ret -> 
		   case lookupFM idmap id of
			Nothing  -> ret id
			Just id' -> ret id'

{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{- A mockup haskell interpreter -}

module Yi.Interpreter (
                       UExpr(..),
                       parse, rename, interpret, toMono) where

import Data.Dynamic
import Control.Monad.Error ()
import Data.Maybe
import Text.ParserCombinators.Parsec.Language (haskell)
import Control.Applicative
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (chainl1)
import qualified Data.Map as M
import Data.Traversable
import Prelude hiding (foldl, mapM)
import Data.Foldable
import GHC.Base (Any)
import Unsafe.Coerce

type Var = String

-- Big hurdle: supporting constraints.
-- unsafeCoerce show ~~> ambiguous type variable. Thus we need a 
data Type = Poly Var [Constraint] Type
          | TyApp Type Type
          | TyCon TyCon
          | TyVar Var

fromMono tr  = mkTyConApp' tc (fmap fromMono args)
    where (tc,args) = splitTyConApp tr


funTyCon = fst $ splitTyConApp $ typeOf (id :: Char -> Char)

mkTyConApp' tyCon = foldl TyApp (TyCon tyCon) 

mkFunTy t1 t2 = mkTyConApp funTyCon [t1,t2]


-- funResultTy' t1 t2 = case unify t1 (mkFunTy t2 (TyVar "frt")) of
funResultTy' = error "funResultTy'"



data Constraint = Constraint TypeClass [Type]

data TypeClass = Action

-- | Polymorphic dynamic type
data PolyDyn = PolyDyn Type Any 

subst :: Var ->Type ->Type -> Type
subst v s t0 = case t0 of
    (Poly v' cs t) -> if v == v' then t0 else Poly v' (fmap substConstraint cs) (subst v s t)
    (TyApp t1 t2) -> TyApp (subst v s t1) (subst v s t2)
    (TyVar v') -> if v == v' then s else t0
    _ -> t0
    where substConstraint (Constraint tc ts) = Constraint tc (fmap (subst v s) ts)




data Atom = AVar String
          | AString String
          | AChar Char
          | AInt Int

instance Show Atom where
    show (AInt i) = show i
    show (AVar s) = s
    show (AString s) = show s
    show (AChar s) = show s

type Env = M.Map String [Dynamic]
type Env' = M.Map String PolyDyn

-- TODO: parens
patom :: Parsec.CharParser st Atom
patom = lexeme haskell (    AVar    <$> identifier     haskell
                        <|> AString <$> stringLiteral  haskell
                        <|> AChar   <$> charLiteral    haskell
                        <|> (AInt . fromIntegral) <$> integer        haskell)

pexpr :: Parsec.CharParser st (UExpr Atom)
pexpr = chainl1 (UVal <$> patom) (pure UApp)

parse ::  (Monad m) => [Char] -> m (UExpr Atom)
parse s = case Parsec.parse pexpr "interactive" s of
            Left err -> fail (show err)
            Right x -> return x
          

data UExpr a where
    UVal :: a -> UExpr a
    UApp :: UExpr a -> UExpr a -> UExpr a

instance Functor UExpr where
    fmap = fmapDefault

instance Foldable UExpr where
    foldMap = foldMapDefault

instance Traversable UExpr where
    traverse f (UVal a) = UVal <$> f a
    traverse f (UApp l r) = UApp <$> traverse f l <*> traverse f r

instance Show a => Show (UExpr a) where
    showsPrec _ (UVal a) = shows a
    showsPrec p (UApp f a) = showParen (p > 0) 
                             (showsPrec 0 f . showChar ' ' . showsPrec 1 a)

type Err = String

rename :: Env -> UExpr Atom -> Either Err (UExpr [Dynamic])
rename env = mapM renameOne
    where
      renameOne (AVar v) = do
        case M.lookup v env of
          Just x -> Right x
          Nothing -> Left $ v ++ " not found in the environment"
      renameOne (AChar x) = box x
      renameOne (AString x) = box x
      renameOne (AInt x) = box x
      box x = return [toDyn x]

rename' :: Env' -> UExpr Atom -> Either Err (UExpr PolyDyn)
rename' env = mapM renameOne
    where
      renameOne (AVar v) = do
        case M.lookup v env of
          Just x -> Right x
          Nothing -> Left $ v ++ " not found in the environment"
      renameOne (AChar x) = box x
      renameOne (AString x) = box x
      renameOne (AInt x) = box x
      box :: Typeable a => a -> Either err PolyDyn
      box = Right . polyBox

polyBox :: Typeable a => a -> PolyDyn
polyBox x = PolyDyn (fromMono $ typeOf x) (unsafeCoerce x)


interpret :: UExpr [Dynamic] -> Either Err [Dynamic]
interpret (UVal da) = return $ da
interpret (UApp df da) = do
  t1 <- interpret df
  t2 <- interpret da
  return $ catMaybes [dynApply f a | f <- t1, a <- t2]


dynApply' (PolyDyn t1 f) (PolyDyn t2 x) =
  case funResultTy' t1 t2 of
    Just t3 ->Just (PolyDyn t3 ((unsafeCoerce f) x))
    Nothing ->Nothing


interpret' (UVal da) = return $ da
interpret' (UApp df da) = do
  f <- interpret' df
  a <- interpret' da
  dynApply' f a



toMono :: forall a. Typeable a => [Dynamic] -> Either Err a
toMono rs = case catMaybes $ map fromDynamic rs of
           [] -> Left $ "value doesn't have type " ++ show (typeOf (undefined::a))
           [r] -> Right r
           _ -> error "eval': ambiguous types"

-- eval :: Env -> String -> Either Err [Dynamic]
-- eval env s =  interpret =<< rename env =<< parse s 

-- test :: String -> Maybe String
-- test s = case eval symTable s of
--            Right [result] -> fromDynamic result
--            Right [] -> Just "type error"
--            Right _ -> Just "ambiguous"
--            Left err -> Just err
--    where 
--         symTable = M.fromList [("one", [toDyn (1::Int)]),
--                                ("show",[toDyn (show :: Int -> String)]) ]


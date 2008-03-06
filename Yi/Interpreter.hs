{- A mockup haskell interpreter -}

module Yi.Interpreter (eval)

import Data.Typeable
import Data.Dynamic
import Control.Monad.Error ()
import Control.Monad (ap)
import Data.Maybe
import Text.ParserCombinators.Parsec.Language (haskell)
import Control.Applicative
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (GenParser, chainl1)
import qualified Data.Map as M
import Data.Traversable
import Prelude hiding (mapM)
import Data.Foldable

instance Alternative (GenParser tok st) where
    (<|>) = (Parsec.<|>)
    empty = fail "no choice"

instance Applicative (GenParser tok st) where
    (<*>) = ap
    pure = return


data Atom = AVar String
          | AString String
          | AChar Char
          | AInt Integer

instance Show Atom where
    show (AVar s) = s
    show (AString s) = show s
    show (AChar s) = show s

type Env = M.Map String Dynamic

-- TODO: parens
patom = lexeme haskell (    AVar    <$> identifier     haskell
                        <|> AString <$> stringLiteral  haskell
                        <|> AChar   <$> charLiteral    haskell
                        <|> AInt    <$> integer        haskell)

pexpr = chainl1 (UVal <$> patom) (pure UApp)

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
    showsPrec p (UVal a) = shows a
    showsPrec p (UApp f a) = showParen (p > 0) 
                             (showsPrec 0 f . showChar ' ' . showsPrec 1 a)

type Err = String

rename :: Env -> UExpr Atom -> Either Err (UExpr Dynamic)
rename env = mapM renameOne
    where
      renameOne :: Atom -> Either Err Dynamic
      renameOne (AVar v) = do
        val <- M.lookup v env
        return val
      renameOne (AChar x) = return (toDyn x)
      renameOne (AString x) = return (toDyn x)
      renameOne (AInt x) = return (toDyn x)

typecheck :: UExpr Dynamic -> Either Err TypeRep
typecheck (UVal da) = return $ dynTypeRep da
typecheck (UApp df da) = do
  t1 <- typecheck df
  t2 <- typecheck da
  case funResultTy t1 t2 of
    Just t3 -> return t3
    Nothing -> fail $ "Can't apply!"

evalExpr :: UExpr Dynamic -> Dynamic
evalExpr (UVal da) = da
evalExpr (UApp df da) = dynApp (evalExpr df) (evalExpr da)
    

eval :: Env -> String -> Either Err Dynamic
eval env s = do
  r <- rename env =<< parse s
  typecheck r
  return $ evalExpr r

test :: Typeable a => a -> String -> a
test a s = fromDyn result a
  where Right result = interpret symTable s
        symTable = M.fromList [("one", toDyn (1::Int))]

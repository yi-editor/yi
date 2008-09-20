{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{- A mockup haskell interpreter -}

module Yi.Interpreter (
                       UExpr(..),
                       parse, rename, interpret, toMono) where

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
        val <- M.lookup v env
        return val
      renameOne (AChar x) = box x
      renameOne (AString x) = box x
      renameOne (AInt x) = box x
      box x = return [toDyn x]

interpret :: UExpr [Dynamic] -> Either Err [Dynamic]
interpret (UVal da) = return $ da
interpret (UApp df da) = do
  t1 <- interpret df
  t2 <- interpret da
  return $ catMaybes [dynApply f a | f <- t1, a <- t2]


eval :: Env -> String -> Either Err [Dynamic]
eval env s =  interpret =<< rename env =<< parse s 

toMono :: forall a. Typeable a => [Dynamic] -> Either Err a
toMono rs = case catMaybes $ map fromDynamic rs of
           [] -> Left $ "value doesn't have type " ++ show (typeOf (undefined::a))
           [r] -> Right r
           _ -> error "eval': ambiguous types"
    


test :: String -> Maybe String
test s = case eval symTable s of
           Right [result] -> fromDynamic result
           Right [] -> Just "type error"
           Right _ -> Just "ambiguous"
           Left err -> Just err
           
   where 
        symTable = M.fromList [("one", [toDyn (1::Int)]),
                               ("show",[toDyn (show :: Int -> String)])
                              ]


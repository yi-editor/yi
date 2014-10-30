{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- TODO:
-- ! User configuration.
-- ! Checking for side-effect-less code, e.g. "1;".

module Yi.Verifier.JavaScript where

import Control.Monad.Writer.Lazy (Writer, MonadWriter, tell)
import Control.Monad
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.DList as D
import Yi.Lexer.Alex (Posn, Tok, tokT, tokPosn)
import Yi.Lexer.JavaScript (Token(..), TT)
import Yi.Syntax.JavaScript hiding (res)


-- * Types

data Error = MultipleFunctionDeclaration String [Posn]
             deriving Eq

data Warning = UnreachableCode Posn
               deriving Eq

data Report = Err  Error
            | Warn Warning
              deriving Eq


-- * Instances

instance Show Error where
    show (MultipleFunctionDeclaration n ps) =
        "Function `" ++ n ++ "' declared more than once: " ++ intercalate ", " (map show ps)

instance Show Warning where
    show (UnreachableCode pos) =
        "Unreachable code at " ++ show pos

instance Show Report where
    show (Err e) = "EE " ++ show e
    show (Warn w) = "WW " ++ show w


-- * Main code

-- | The main verifier which calls the sub-verifiers.
verify :: Tree TT -> Writer (D.DList Report) ()
verify t = do
  let topfuns = findFunctions (toList t)
  checkMultipleFuns topfuns
  mapM_ (checkUnreachable . funBody) topfuns

-- | Given a list of function declarations, checks for multiple function
--   declarations, including the functions' subfunctions.
checkMultipleFuns :: [Statement TT] -> Writer (D.DList Report) ()
checkMultipleFuns stmts = do
  let dupFuns = dupsBy (ttEq `on` funName) stmts
  unless (null dupFuns)
    (say (Err (MultipleFunctionDeclaration
               (nameOf $ tokT $ funName $ head dupFuns)
               (map (tokPosn . funName) dupFuns))))
  let subFuns = map (findFunctions . funBody) (findFunctions stmts)
  mapM_ checkMultipleFuns subFuns

checkUnreachable :: [Statement TT] -> Writer (D.DList Report) ()
checkUnreachable stmts = do
  let afterReturn = dropWhile' (not . isReturn) stmts
  unless (null afterReturn)
    (say (Warn (UnreachableCode (tokPosn $ firstTok $ head afterReturn))))


-- * Helper functions

-- | Given two @Tok t@, compares the @t@s.
ttEq :: Eq t => Tok t -> Tok t -> Bool
ttEq x y = tokT x == tokT y

say :: MonadWriter (D.DList a) m => a -> m ()
say = tell . D.singleton

isReturn :: Statement t -> Bool
isReturn (Return {}) = True
isReturn _           = False

-- | Returns a list of the functions in the given block.
findFunctions :: [Statement t] -> [Statement t]
findFunctions stmts = [ f | f@(FunDecl {}) <- stmts ]

-- | Given a 'FunDecl', returns the token representing the name.
funName :: Statement t -> t
funName (FunDecl _ n _ _) = n
funName _                 = undefined

-- | Given a 'FunDecl', returns its inner body as a list.
funBody :: Statement t -> [Statement t]
funBody (FunDecl _ _ _ blk) =
    case blk of
      Block _ stmts _ -> toList stmts
      BlockOne stmt   -> [stmt]
      _               -> []
funBody _ = undefined

-- | Given a @ValidName@ returns the string representing the name.
nameOf :: Token -> String
nameOf (ValidName n) = n
nameOf _             = undefined


-- * Misc

-- | Like 'dropWhile' but drops the first element in the result.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs =
    let res = dropWhile p xs in
    if null res then [] else drop 1 res

dupsBy :: (a -> a -> Bool) -> [a] -> [a]
dupsBy p xs = filter (\x -> length (filter (p x) xs) > 1) xs


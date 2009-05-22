{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Yi.Verifier.JavaScript where

import Control.Monad.Writer.Lazy (Writer, mapM_, MonadWriter, tell)
import Data.List (map, dropWhile, drop, filter, length)
import qualified Data.DList as D
import Prelude ()
import Yi.Lexer.Alex (Posn, Tok, tokT, tokPosn)
import Yi.Lexer.JavaScript (Token(..), TT)
import Yi.Prelude hiding (mapM_)
import Yi.Syntax.JavaScript hiding (res)


-- * Types

data ErrType = MultipleFunctionDeclaration String [Posn]
               deriving Eq

data WarnType = UnreachableCode Posn
                deriving Eq

data Report = Err  ErrType
            | Warn WarnType
              deriving Eq


-- * Instances

-- | A real liar of an Eq instance.  Only compares the @Token@ in @Tok Token@.
instance Eq (Tok Token) where
    x == y = tokT x == tokT y

instance Show ErrType where
    show (MultipleFunctionDeclaration n ps) =
        "Function `" ++ n ++ "' declared more than once: " ++ show ps

instance Show WarnType where
    show (UnreachableCode pos) =
        "Unreachable code at " ++ show pos

instance Show Report where
    show (Err e) = "EE " ++ show e
    show (Warn w) = "WW " ++ show w


-- * Main code

-- | The main verifier which calls the sub-verifiers.
verify :: Tree TT -> Writer (D.DList Report) ()
verify t = do
  let topfuns = findFunctions (toList t) -- top level functions
  log $ "TOP LEVEL FUNCTIONS: " ++ show (map (nameOf . tokT . funName) topfuns)
  checkMultipleFuns topfuns
  mapM_ (checkUnreachable . funBody) topfuns

-- | Given a list of function declarations, checks for multiple function
--   declarations, including the functions' subfunctions.
checkMultipleFuns :: [Statement TT] -> Writer (D.DList Report) ()
checkMultipleFuns stmts = do
  trace ("Checking for multiple function declarations... Statements:\n" ++ show stmts) $ return ()
  let dupFuns = dupsBy ((==) `on` funName) stmts
  when (not $ null dupFuns)
    (say (Err (MultipleFunctionDeclaration
               (nameOf $ tokT $ funName $ head dupFuns)
               (map (tokPosn . funName) dupFuns))))
  let subFuns = map (findFunctions . funBody) (findFunctions stmts)
  mapM_ checkMultipleFuns subFuns

checkUnreachable :: [Statement TT] -> Writer (D.DList Report) ()
checkUnreachable stmts = do
  log $ "Checking unreachable code... Statements:\n" ++ show stmts
  let afterReturn = dropWhile' (not . isReturn) stmts
  when (not (null afterReturn))
    (say (Warn (UnreachableCode (tokPosn $ firstTok $ head afterReturn))))


-- * Helper functions

log :: Monad m => String -> m ()
log x = trace x $ return ()

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
funName _                 = trace "UNDEFINED: funName" undefined

-- | Given a 'FunDecl', returns its inner body as a list.
funBody :: Statement t -> [Statement t]
funBody (FunDecl _ _ _ blk) =
    case blk of
      Block _ stmts _ -> toList stmts
      BlockOne stmt   -> [stmt]
      _               -> []
funBody _ = trace "UNDEFINED: funBody" undefined

-- | Given a @ValidName@ returns the string representing the name.
nameOf :: Token -> String
nameOf (ValidName n) = n
nameOf _             = trace "UNDEFINED: nameOf" undefined


-- * Misc

-- | Like 'dropWhile' but drops the first element in the result.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs =
    let res = dropWhile p xs in
    if null res then [] else drop 1 res

dupsBy :: (a -> a -> Bool) -> [a] -> [a]
dupsBy p xs = filter (\x -> length (filter (p x) xs) > 1) xs

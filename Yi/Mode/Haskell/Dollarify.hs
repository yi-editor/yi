{-# LANGUAGE PatternGuards #-}
module Yi.Mode.Haskell.Dollarify where

import Prelude (filter, maybe, takeWhile, uncurry)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Yi.Prelude 
import Yi.Syntax.Paren (Expr, Tree(..))
import Yi.Syntax.Tree (getAllSubTrees, getFirstOffset, getLastOffset,getLastPath)
import Yi.Lexer.Alex (posnOfs, Tok(..))
import Yi.Lexer.Haskell (isComment, TT, Token(..))
import Yi.Buffer hiding (Block)

dollarify :: Expr TT -> BufferM ()
dollarify e = maybe (return ()) dollarifyWithin . selectedTree e =<< getSelectRegionB

dollarifyWithin :: Tree TT -> BufferM ()
dollarifyWithin = trace . ("dollarifyWithin: " ++) . show <*> runQ . (dollarifyTop =<<) . getAllSubTrees

data QueuedUpdate = QueuedUpdate { qUpdatePoint :: Point
                                 , qInsert      :: String
                                 , qDelete      :: Int
                                 } deriving (Eq, Ord)

runQ :: [QueuedUpdate] -> BufferM ()
runQ = mapM_ run1Q . sortBy (flip compare)
    where
       run1Q :: QueuedUpdate -> BufferM ()
       run1Q (QueuedUpdate { qUpdatePoint = p, qInsert = i, qDelete = d })
              = do deleteNAt Forward d p
                   when (not $ null i) $ insertNAt i p

openParen, closeParen :: Token
openParen = Special '('
closeParen = Special ')'

isNormalParen :: Tree TT -> Bool
isNormalParen (Paren t1 _ t2) = tokT t1 == openParen && tokT t2 == closeParen
isNormalParen _               = False

-- Assumes length of token is one character
queueDelete :: TT -> QueuedUpdate
queueDelete = queueReplaceWith ""

-- Assumes length of token is one character
queueReplaceWith :: String -> TT -> QueuedUpdate
queueReplaceWith s t = QueuedUpdate { qUpdatePoint = posnOfs $ tokPosn t
                                    , qInsert = s
                                    , qDelete = 1
                                    }

-- Only strips comments from the top level
stripComments :: Expr TT -> Expr TT
stripComments = filter $ \t -> case t of { (Atom x) -> not (isComment $ tokT x); _ -> True }

dollarifyTop :: Tree TT -> [QueuedUpdate]
dollarifyTop p@(Paren t1 e t2)
   | isNormalParen p = case stripComments e of
       [Paren _ _ _] -> [queueDelete t2, queueDelete t1]
       e'            -> dollarifyExpr e'
dollarifyTop (Block bList) = dollarifyExpr . stripComments =<< toList bList
dollarifyTop _ = []

-- Expression must not contain comments
dollarifyExpr :: Expr TT -> [QueuedUpdate]
dollarifyExpr e@(_:_) | p@(Paren t e2 t2) <- last e
                     , isNormalParen p
                     , isCollapsible e2
                     , all isSimple e
   = [queueDelete t2, queueReplaceWith "$ " t]
dollarifyExpr _ = []

isSimple :: Tree TT -> Bool
isSimple (Paren _ _ _) = True
isSimple (Block _)     = False
isSimple (Atom t)      = tokT t `elem` [Number, CharTok, StringTok,VarIdent, ConsIdent]
isSimple _             = False

both :: (a -> b) -> a -> a -> (b, b)
both f x y = (f x, f y)

isCollapsible :: Expr TT -> Bool
isCollapsible = uncurry (&&) . (both isSimple . head <*> last) . stripComments

selectedTree :: Expr TT -> Region -> Maybe (Tree TT)
selectedTree e r = findLargestWithin r <$> getLastPath e (regionLast r)

-- List must be non-empty
findLargestWithin :: Region -> [Tree TT] -> Tree TT
findLargestWithin r = fromMaybe . head <*> safeLast . takeWhile (within r)

within :: Region -> Tree TT -> Bool
within r t = includedRegion ((mkRegion . getFirstOffset <*> getLastOffset) t) r

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast s  = return $ last s
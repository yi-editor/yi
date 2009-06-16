{-# LANGUAGE PatternGuards #-}
module Yi.Mode.Haskell.Dollarify where

import Prelude (filter, maybe, takeWhile)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Yi.Prelude 
import Yi.Syntax.Paren (Expr, Tree(..))
import qualified Yi.Syntax.Haskell as H (Tree(..), Exp(..), getExprs)
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
                                 } deriving (Eq, Ord, Show)

runQ :: [QueuedUpdate] -> BufferM ()
runQ = trace . ("runQ: " ++) . show <*> mapM_ run1Q . sortBy (flip compare)
    where
       run1Q :: QueuedUpdate -> BufferM ()
       run1Q (QueuedUpdate { qUpdatePoint = p, qInsert = i, qDelete = d })
              = do deleteNAt Forward d p
                   when (not $ null i) $ insertNAt i p

openParen, closeParen :: Token
openParen = Special '('
closeParen = Special ')'

isNormalParen :: Tree TT -> Bool
isNormalParen (Paren t1 xs t2) = tokT t1 == openParen && tokT t2 == closeParen && (not $ any isTuple xs)
isNormalParen _               = False

isTuple ::Tree TT -> Bool
isTuple (Atom t) = tokT t == Special ','
isTuple _ = False

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
dollarifyExpr e@(_:_)
    | p@(Paren t e2 t2) <- last e
    , isNormalParen p
    , all isSimple e
    = let dollarifyLoop :: Expr TT -> [QueuedUpdate]
          dollarifyLoop [] = []
          dollarifyLoop e3@[Paren _ _ _] = dollarifyExpr e3
          dollarifyLoop e3 = if isCollapsible e3 then [queueDelete t2, queueReplaceWith "$ " t] else []
          in dollarifyLoop $ stripComments e2
dollarifyExpr _ = []

isSimple :: Tree TT -> Bool
isSimple (Paren _ _ _) = True
isSimple (Block _)     = False
isSimple (Atom t)      = tokT t `elem` [Number, CharTok, StringTok, VarIdent, ConsIdent]
isSimple _             = False

-- Expression must not contain comments
isCollapsible :: Expr TT -> Bool
isCollapsible = (((&&) `on` isSimple) . head <*> last)

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

-- Here follows code for the precise haskell mode

dollarifyP :: H.Tree TT -> BufferM ()
dollarifyP e = maybe (return ()) dollarifyWithinP . selectedTreeP (H.getExprs e) =<< getSelectRegionB

dollarifyWithinP :: H.Exp TT -> BufferM ()
dollarifyWithinP = trace . ("dollarifyWithin: " ++) . show <*> runQ . (dollarifyTopP =<<) . getAllSubTrees

isNormalParenP :: H.Exp TT -> Bool
isNormalParenP (H.Paren (H.PAtom r _) xs (H.PAtom r' _)) = tokT r == openParen && tokT r' == closeParen && (not $ any isTupleP xs)
isNormalParenP _               = False

isTupleP :: H.Exp TT -> Bool
isTupleP (H.PAtom t _) = tokT t == Special ','
isTupleP _ = False

-- Only strips comments from the top level
stripCommentsP :: [H.Exp TT] -> [H.Exp TT]
stripCommentsP = filter $ \t -> case t of { (H.PAtom x _) -> not (isComment $ tokT x); _ -> True }

dollarifyTopP :: H.Exp TT -> [QueuedUpdate]
dollarifyTopP p@(H.Paren (H.PAtom t1 _) e (H.PAtom t2 _))
   | isNormalParenP p = case stripCommentsP e of
       [H.Paren _ _ _] -> [queueDelete t2, queueDelete t1]
       e'            -> dollarifyExprP e'
dollarifyTopP (H.Block bList) = dollarifyExprP . stripCommentsP =<< toList bList
dollarifyTopP _ = []

-- Expression must not contain comments
dollarifyExprP :: [H.Exp TT] -> [QueuedUpdate]
dollarifyExprP e@(_:_)
    | p@(H.Paren (H.PAtom t _) e2 (H.PAtom t2 _)) <- last e
    , isNormalParenP p
    , all isSimpleP e
    = let dollarifyLoop :: [H.Exp TT] -> [QueuedUpdate]
          dollarifyLoop [] = []
          dollarifyLoop e3@[H.Paren _ _ _] = dollarifyExprP e3
          dollarifyLoop e3 = if isCollapsibleP e3 then [queueDelete t2, queueReplaceWith "$ " t] else []
          in dollarifyLoop $ stripCommentsP e2
dollarifyExprP _ = []

isSimpleP :: H.Exp TT -> Bool
isSimpleP (H.Paren _ _ _) = True
isSimpleP (H.Block _)     = False
isSimpleP (H.PAtom t _)      = tokT t `elem` [Number, CharTok, StringTok, VarIdent, ConsIdent]
isSimpleP _             = False

-- Expression must not contain comments
isCollapsibleP :: [H.Exp TT] -> Bool
isCollapsibleP = (((&&) `on` isSimpleP) . head <*> last)

selectedTreeP :: [H.Exp TT] -> Region -> Maybe (H.Exp TT)
selectedTreeP e r = findLargestWithinP r <$> getLastPath e (regionLast r)

-- List must be non-empty
findLargestWithinP :: Region -> [H.Exp TT] -> H.Exp TT
findLargestWithinP r = fromMaybe . head <*> safeLast . takeWhile (withinP r)

withinP :: Region -> H.Exp TT -> Bool
withinP r t = includedRegion ((mkRegion . getFirstOffset <*> getLastOffset) t) r
    
safeLastP :: [a] -> Maybe a
safeLastP [] = Nothing
safeLastP s  = return $ last s
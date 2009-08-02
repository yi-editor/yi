{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{- Copyright JP Bernardy 2008 -}

-- | Generic syntax tree handling functions
module Yi.Syntax.Tree where

-- Some of this might be replaced by a generic package
-- such as multirec, uniplace, emgm, ...

import Data.List (dropWhile, takeWhile, reverse)
import Data.Maybe
import Data.Monoid
import Prelude ()
import Yi.Buffer.Basic
import Yi.Lexer.Alex
import Yi.Prelude
import Yi.Region

class Foldable tree => IsTree tree where
    -- | Direct subtrees of a tree
    subtrees :: tree t -> [tree t]

class SubTree tree where
    type Element tree
    foldMapToksAfter :: Monoid m => Point -> (Element tree -> m) ->tree ->m
    foldMapToks :: Monoid m => (Element tree -> m) ->tree ->m


instance SubTree (Tok a) where
    type Element (Tok a) = Tok a
    foldMapToksAfter _begin f t = f t
    foldMapToks f t = f t


instance SubTree t => SubTree [t] where
    type Element [t] = Element t
    foldMapToksAfter begin f = foldMap (foldMapToksAfter begin f)
    foldMapToks f = foldMap (foldMapToks f)

toksAfter :: SubTree tree => Point -> tree -> [Element tree]
toksAfter begin t = foldMapToksAfter begin (:) t []

allToks :: SubTree tree =>tree -> [Element tree]
allToks t = foldMapToks (:) t []

tokAtOrBefore :: (Element a ~ Tok t, SubTree a) => Point -> a -> Maybe (Tok t)
tokAtOrBefore p res = listToMaybe $ reverse $ toksInRegion (mkRegion 0 (p+1)) res

toksInRegion :: (Element a ~ Tok t, SubTree a) => Region -> a -> [Tok t]
toksInRegion reg = takeWhile (\t -> tokBegin t <= regionEnd   reg) . dropWhile (\t -> tokEnd t < regionStart reg) . toksAfter (regionStart reg)


tokenBasedAnnots :: SubTree tree =>(Element tree ->Maybe a) -> tree -> Point -> [a]
tokenBasedAnnots tta t begin = catMaybes $ fmap tta $ toksAfter begin t

tokenBasedStrokes :: SubTree tree =>(Element tree -> a) -> tree -> Point -> Point -> Point -> [a]
tokenBasedStrokes tts t _point begin _end = foldMapToksAfter begin (\x ->(tts x:)) t []

{-

-- | Given a path to a leaf which is at the end or after the window, return a 
-- node that encompasses the current window, and a path to a leaf
-- which is at the end, or after the current window.
fromLeafAfterToFinal :: Region -> Path -> tree t -> (Path, tree t)


-- | Given a path to a leaf, return a path to a leaf which is at or after the given point
fromLeafToLeafAfter :: Point -> Path -> tree t -> (Path, tree t)
fromLeafToLeafAfter p xs t = firstThat (\s -> getFirstOffset s > p) $ allLeavesAfter xs t

-- | Takes a list of (node, index of already inspected child), and return all leaves
-- in this node after the said child).
allLeavesAfter :: Path -> tree t -> [(Path, tree t)]
allLeavesAfter xs t = concat $ zipWith fixPath $ map $ reverse $ allLeavesAfterChild xs t

-- | Given a path, return all the nodes encountered along it, and the index of the child which comes next.
nodesAndChildIndex :: Path -> tree t -> [(tree t, Int)]
nodesAndChildIndex [] t = [(t,negate 1)]
nodesAndChildIndex (x:xs) = case c of
    Just c' -> (t, x) : nodesAndChildIndex xs c'
    Nothing -> [(t,negate 1)]
  where c = index x (subtrees t)
          

allLeavesAfterChild :: (tree t,Int) -> [(Path, tree t)]
allLeavesAfterChild t x 
    | null ts = [([], t)]
    | otherwise = concat $ map allLeavesIn $ drop (x+1) $ subtrees t
    where ts = subtrees t


allLeavesIn :: tree t -> [(Path, tree t)]
allLeavesIn t 
    | null ts = [([], t)]
    | otherwise = concat $ zipWith (\(xs,t') x -> (x:xs,t')) ts [0..]
    where ts = subtrees t

-}

-- | Return all subtrees in a tree; each element of the return list
-- contains paths to nodes. (Root is at the start of each path)
getAllPaths :: IsTree tree => tree t -> [[tree t]]
getAllPaths t = fmap (++[t]) ([] : concatMap getAllPaths (subtrees t))

-- | Search the given list, and return the last tree before the given
-- point; with path to the root. (Root is at the start of the path)
getLastPath :: IsTree tree => [tree (Tok t)] -> Point -> Maybe [tree (Tok t)]
getLastPath roots offset =
    case takeWhile ((< offset) . posnOfs . snd) allSubPathPosn of
      [] -> Nothing
      xs -> Just $ fst $ last xs
    where allSubPathPosn = [(p,posn) | root <- roots, p@(t':_) <- getAllPaths root, 
                            Just tok <- [getFirstElement t'], let posn = tokPosn tok]


-- | Return all subtrees in a tree, in preorder.
getAllSubTrees :: IsTree tree => tree t -> [tree t]
getAllSubTrees t = t : concatMap getAllSubTrees (subtrees t)


-- | Return the 1st token of a subtree.
getFirstElement :: Foldable t => t a -> Maybe a
getFirstElement tree = getFirst $ foldMap (First . Just) tree

getFirstTok, getLastTok :: (SubTree a) => a -> Maybe (Element a)
getFirstTok = getFirst . foldMapToks (First . Just) 
getLastTok = getLast . foldMapToks (Last . Just) 

-- | Return the last token of a subtree.
getLastElement :: Foldable t => t a -> Maybe a
getLastElement tree = getLast $ foldMap (Last . Just) tree


getLastOffset :: (Element a ~ Tok t, SubTree a) =>a -> Point
getLastOffset = maybe 0 tokEnd . getLastTok

getFirstOffset :: (Element a ~ Tok t, SubTree a) =>a -> Point
getFirstOffset = maybe 0 tokBegin . getFirstTok

-- | Given a tree, return (first offset, number of lines).
getSubtreeSpan :: (Foldable tree) => tree (Tok t) -> (Point, Int)
getSubtreeSpan tree = (posnOfs $ first, lastLine - firstLine)
    where bounds@[first, _last] = fmap (tokPosn . assertJust) [getFirstElement tree, getLastElement tree]
          [firstLine, lastLine] = fmap posnLine bounds
          assertJust (Just x) = x
          assertJust _ = error "assertJust: Just expected"


-------------------------------------
-- Should be in Control.Applicative.?

sepBy :: (Alternative f) => f a -> f v -> f [a]
sepBy p s   = sepBy1 p s <|> pure []

sepBy1     :: (Alternative f) => f a -> f v -> f [a]
sepBy1 p s  = (:) <$> p <*> many (s *> p)

{-# LANGUAGE TypeFamilies #-}
{- Copyright JP Bernardy 2008 -}

-- | Generic syntax tree handling functions
module Yi.Syntax.Tree where

-- Some of this might be replaced by a generic package
-- such as multirec, uniplace, emgm, ...

import Data.List (takeWhile, reverse)
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



instance SubTree t => SubTree [t] where
    type Element [t] = Element t
    foldMapToksAfter begin f = foldMap (foldMapToksAfter begin f)

toksAfter :: SubTree tree =>Point -> tree -> [Element tree]
toksAfter begin t = foldMapToksAfter begin (\x ->(x:)) t []

tokAtOrBefore p res = listToMaybe $ reverse $ toksInRegion (mkRegion 0 (p+1)) res

toksInRegion reg = takeWhile (\t -> tokBegin t <= regionEnd   reg) . toksAfter (regionStart reg)


tokenBasedAnnots :: SubTree tree =>(Element tree ->Maybe a) -> tree -> Point -> [a]
tokenBasedAnnots tta t begin = catMaybes $ fmap tta $ toksAfter begin t

tokenBasedStrokes :: SubTree tree =>(Element tree -> a) -> tree -> Point -> Point -> Point -> [a]
tokenBasedStrokes tts t _point begin _end = foldMapToksAfter begin (\x ->(tts x:)) t []


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
getFirstElement tree = getFirst $ foldMap (\x -> First (Just x)) tree

-- | Return the last token of a subtree.
getLastElement :: Foldable t => t a -> Maybe a
getLastElement tree = getLast $ foldMap (\x -> Last (Just x)) tree


getLastOffset :: (Foldable t) => t (Tok tok) -> Point
getLastOffset = maybe 0 tokEnd . getLastElement


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

{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables #-}
{- Copyright JP Bernardy 2008 -}

-- | Generic syntax tree handling functions
module Yi.Syntax.Tree (IsTree(..), toksAfter, allToks, tokAtOrBefore, toksInRegion,
                       sepBy, sepBy1,
                       getLastOffset, getFirstOffset,
                       getFirstElement, getLastElement,
                       getLastPath,
                       getAllSubTrees,
                       tokenBasedAnnots, tokenBasedStrokes,
                       subtreeRegion,
                       fromLeafToLeafAfter, fromNodeToFinal) 
  where

-- Some of this might be replaced by a generic package
-- such as multirec, uniplace, emgm, ...

import Control.Monad (ap)
import Data.List (dropWhile, takeWhile, reverse, filter, zip, zipWith, take, drop, length, splitAt)
import Data.Maybe
import Data.Monoid
import Prelude (curry)
import Yi.Buffer.Basic
import Yi.Lexer.Alex
import Yi.Prelude
import Yi.Region
import Data.Accessor.Tuple
import Test.QuickCheck


class Foldable tree => IsTree tree where
    -- | Direct subtrees of a tree
    subtrees :: tree t -> [tree t]
    subtrees = fst . uniplate
    uniplate :: tree t -> ([tree t], [tree t] -> tree t)
    emptyNode :: tree t

toksAfter _begin = allToks

allToks = toList

tokAtOrBefore p res = listToMaybe $ reverse $ toksInRegion (mkRegion 0 (p+1)) res

toksInRegion reg = takeWhile (\t -> tokBegin t <= regionEnd   reg) . dropWhile (\t -> tokEnd t < regionStart reg) . toksAfter (regionStart reg)


tokenBasedAnnots :: (Foldable t1) => (a1 -> Maybe a) -> t1 a1 -> t -> [a]
tokenBasedAnnots tta t begin = catMaybes $ fmap tta $ toksAfter begin t


tokenBasedStrokes :: (Foldable t3) => (a -> b) -> t3 a -> t -> t2 -> t1 -> [b]
tokenBasedStrokes tts t _point begin _end = fmap tts $ toksAfter begin t

-- | Prune the nodes before the given point.
-- The path is used to know which nodes we can force or not.
pruneNodesBefore :: IsTree tree => Point -> Path -> tree (Tok t) -> tree (Tok t)
pruneNodesBefore p [] t = t
pruneNodesBefore p (x:xs) t = rebuild (left' ++ pruneNodesBefore p xs c : rights)
    where (children,rebuild) = uniplate t
          (left,c:rights) = splitAt x children
          left' = fmap replaceEmpty left
          replaceEmpty s = if getLastOffset s < p then emptyNode else s

-- | Given an approximate path to a leaf at the end of the region, return:
-- (path to leaf at the end of the region,path from focused node to the leaf, small node encompassing the region)
fromNodeToFinal :: IsTree tree => Region -> Node (tree (Tok t)) -> (Path,tree (Tok t))
fromNodeToFinal r (xs,root) = 
    trace ("r = " ++ show r) $
    trace ("focused ~ " ++ show (subtreeRegion focused) ) $
    trace ("pathFromFocusedToLeaf = " ++ show focusedToLeaf) $
    trace ("pruned ~ " ++ show (subtreeRegion focused) ) $


    (xs',pruned)
    where n@(xs',_) = fromLeafToLeafAfter (regionEnd r) (xs,root)
          (_,(focusedToLeaf,focused)) = fromLeafAfterToFinal p0 n
          p0 = regionStart r
          pruned = pruneNodesBefore p0 focusedToLeaf focused

-- | Return the first element that matches the predicate, or the last of the list
-- if none matches.
firstThat p [] = error "firstThat: empty list"
firstThat p [x] = x
firstThat p (x:xs) = if p x then x else firstThat p xs

-- | Return the element before first element that violates the predicate, or the first of the list
-- if that one violates the predicate.
lastThat p [] = error "lastThat: empty list"
lastThat p (x:xs) = if p x then work x xs else x
    where work x0 [] = x0
          work x0 (x:xs) = if p x then work x xs else x0

-- | Given a path to a node, return a path+node which 
-- node that encompasses the given node + a point before it.
fromLeafAfterToFinal :: IsTree tree => Point -> Node (tree (Tok t)) -> (Path,Node (tree (Tok t)))
fromLeafAfterToFinal p n = 
    -- trace ("reg = " ++ show (fmap (subtreeRegion . snd) nsPth)) $ 
      firstThat (\(_,(_,s)) -> getFirstOffset s <= p) ns
    where ns = (reverse (nodesOnPath n))

-- | Search the tree in pre-order starting at a given node, until finding a leaf which is at
-- or after the given point. An effort is also made to return a leaf as close as possible to @p@.
-- TODO: rename to fromLeafToLeafAt
fromLeafToLeafAfter :: (IsTree tree) => Point -> Node (tree (Tok t)) -> Node (tree (Tok t))
fromLeafToLeafAfter p (xs,root) = 
    trace "fromLeafToLeafAfter:" $
    trace ("xs = " ++ show xs) $
    trace ("xsValid = " ++ show xsValid) $
    trace ("p = " ++ show p) $
    trace ("leafBeforeP = " ++ show leafBeforeP) $
    trace ("leaf ~ " ++ show (subtreeRegion leaf)) $
    trace ("xs' = " ++ show xs') $
    result
    where xs' = fst $ if leafBeforeP
                      then firstThat (\(_,s) -> getFirstOffset s >= p) $ allLeavesRelative afterChild n
                      else lastThat (\(_,s) -> getFirstOffset s >= p) $ allLeavesRelative beforeChild n
          (xsValid,leaf) = wkDown (xs,root)
          leafBeforeP = getFirstOffset leaf <= p
          n = (xsValid,root)
          result = if nullSubtree root then (xs,root) else (xs',root)
                   -- firstthat/lastthat do not handle properly empty lists, so special-case here.


allLeavesRelative select 
   = filter (not . nullSubtree . snd) . allLeavesRelative' select . reverse . nodesAndChildIndex
     -- we remove empty subtrees because their region is [0,0].


-- | Takes a list of (node, index of already inspected child), and return all leaves
-- in this node after the said child).
-- allLeavesAfter' :: IsTree tree => [(Node (tree t), Int)] -> [(Path, tree t)]
allLeavesRelative' select l 
  = [(xs ++ xs', t') | ((xs,t),c) <- l, (xs',t') <- allLeavesRelativeChild select c t] 

-- | Given a root, return all the nodes encountered along it, their
-- paths, and the index of the child which comes next.
nodesAndChildIndex :: IsTree tree => Node (tree t) -> [(Node (tree t), Int)]
nodesAndChildIndex ([],t) = [(([],t),negate 1)]
nodesAndChildIndex (x:xs, t) = case index x (subtrees t) of
    Just c' -> (([],t), x) : fmap (frst (frst (x:))) (nodesAndChildIndex (xs,c'))
    Nothing -> [(([],t),negate 1)]
          
nodesOnPath :: IsTree tree => Node (tree t) -> [(Path, Node (tree t))]
nodesOnPath ([],t) = [([],([],t))]
nodesOnPath (x:xs,t) = ([],(x:xs,t)) : case index x (subtrees t) of
                           Nothing -> error "nodesOnPath: non-existent path"
                           Just c -> fmap (frst (x:)) (nodesOnPath (xs,c))



frst :: (a -> b) -> (a,c) -> (b,c)
frst f ~(x,y) = (f x, y)


afterChild c = drop (c+1)
beforeChild (-1) = reverse -- (-1) indicates that all children should be taken.
beforeChild c = reverse . take (c-1)


-- allLeavesBeforeChild :: IsTree tree => Int -> tree t -> [(Path, tree t)]
-- Return all leaves after or before child depending on the relation which is given.
allLeavesRelativeChild select c t 
    | null ts = [([], t)]
    | otherwise = [(x:xs,t') | (x,ct) <- select c (zip [0..] ts),
                   (xs, t') <- allLeavesIn select ct]
   where ts = subtrees t                      


-- | Return all leaves (with paths) inside a given root.
-- allLeavesIn :: IsTree tree => tree t -> [Node (tree t)]
allLeavesIn select = allLeavesRelativeChild select (-1)


-- | Return all subtrees in a tree; each element of the return list
-- contains paths to nodes. (Root is at the start of each path)
getAllPaths :: IsTree tree => tree t -> [[tree t]]
getAllPaths t = fmap (++[t]) ([] : concatMap getAllPaths (subtrees t))

goDown :: IsTree tree => Int -> tree t -> Maybe (tree t)
goDown i = index i . subtrees 

index _ [] = Nothing
index 0 (h:_) = Just h
index n (_:t) = index (n-1) t

type Path = [Int]


type Node t = (Path, t)


walkDown :: IsTree tree => Node (tree t) -> Maybe (tree t)
walkDown ([],t) = return t
walkDown (x:xs,t) = goDown x t >>= curry walkDown xs

wkDown ([],t) = ([],t)
wkDown (x:xs,t) = case goDown x t of
    Nothing -> ([],t)
    Just t' -> frst (x:) $ wkDown (xs,t')


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

nullSubtree = null . toList

getFirstTok = getFirstElement
getLastTok = getLastElement

-- | Return the last token of a subtree.
getLastElement :: Foldable t => t a -> Maybe a
getLastElement tree = getLast $ foldMap (Last . Just) tree


getLastOffset = maybe 0 tokEnd . getLastTok
getFirstOffset = maybe 0 tokBegin . getFirstTok


subtreeRegion t = mkRegion (getFirstOffset t) (getLastOffset t) 

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


----------------------------------------------------
-- Testing code.


data Test a = Empty | Leaf a | Bin (Test a) (Test a) deriving (Show, Eq)

instance Foldable Test where
    foldMap f (Leaf x) = f x
    foldMap f (Bin l r) = foldMap f r <> foldMap f r

instance IsTree Test where
    uniplate (Bin l r) = ([l,r],\[l',r'] -> Bin l' r')
    uniplate t = ([],\[] -> t)
    emptyNode = Empty


type TT = Tok ()




instance Arbitrary (Test TT) where
    arbitrary = sized $ \size -> do
      arbitraryFromList [1..size+1]
    shrink (Leaf x) = []
    shrink (Bin l r) = [l,r] ++  (Bin <$> shrink l <*> pure r) ++  (Bin <$> pure l <*> shrink r)

tAt idx =  Tok () 1 (Posn (idx * 2) 0 0)

arbitraryFromList :: [Int] -> Gen (Test TT)
arbitraryFromList [] = error "arbitraryFromList expects non empty lists"
arbitraryFromList [x] = pure (Leaf (tAt (fromIntegral x)))
arbitraryFromList xs = do
  m <- choose (1,length xs - 1)
  let len = length xs
      (l,r) = splitAt m xs
  Bin <$> arbitraryFromList l <*> arbitraryFromList r

instance Eq (Tok a) where
    x == y = tokPosn x == tokPosn y
    
instance Applicative Gen where
    pure = return
    (<*>) = ap

instance Arbitrary Region where
    arbitrary = sized $ \size -> do
        x0 :: Int <- arbitrary
        return $ mkRegion (fromIntegral x0) (fromIntegral (x0 + size))

newtype NTTT = N (Node (Test TT)) deriving Show

instance Arbitrary NTTT where
    arbitrary = do
      t <- arbitrary
      p <- arbitraryPath t
      return $ N (p,t)

arbitraryPath (Leaf _) = return []
arbitraryPath (Bin l r) = do
  c <- choose (0,1)
  let Just n' = index c [l,r]
  (c :) <$> arbitraryPath n'

regionInside r = do
  b :: Int <- choose (fromIntegral $ regionStart r, fromIntegral $ regionEnd r)
  e :: Int <- choose (b, fromIntegral $ regionEnd r)
  return $ mkRegion (fromIntegral b) (fromIntegral e)

pointInside :: Region -> Gen Point
pointInside r = do
  p :: Int <- choose (fromIntegral $ regionStart r, fromIntegral $ regionEnd r)
  return (fromIntegral p)


nodeRegion n = subtreeRegion t
    where Just t = walkDown n


prop_fromLeafAfterToFinal :: NTTT -> Property
prop_fromLeafAfterToFinal (N n) = let
    fullRegion = subtreeRegion $ snd n
 in forAll (pointInside fullRegion) $ \p -> do
   let final@(finalPath, (pathFromSubtree, finalSubtree)) = fromLeafAfterToFinal p n
       finalRegion = subtreeRegion finalSubtree
       initialRegion = nodeRegion n
       
   whenFail (do putStrLn $ "final = " ++ show final
                putStrLn $ "final reg = " ++ show finalRegion
                putStrLn $ "initialReg = " ++ show initialRegion
                putStrLn $ "p = " ++ show p
            ) 
     ((regionStart finalRegion <= p) && (initialRegion `includedRegion` finalRegion))


prop_allLeavesAfter :: NTTT -> Property
prop_allLeavesAfter (N n@(xs,t)) = do
  let after = allLeavesRelative afterChild n
  (xs',t') <- elements after
  let t'' = walkDown (xs',t)
  whenFail (do putStrLn $ "t' = " ++ show t'
               putStrLn $ "t'' = " ++ show t''
               putStrLn $ "xs' = " ++ show xs'
           ) (Just t' == t'' && xs <= xs')
      

prop_allLeavesBefore :: NTTT -> Property
prop_allLeavesBefore (N n@(xs,t)) = do
  let after = allLeavesRelative beforeChild n
  (xs',t') <- elements after
  let t'' = walkDown (xs',t)
  whenFail (do putStrLn $ "t' = " ++ show t'
               putStrLn $ "t'' = " ++ show t''
               putStrLn $ "xs' = " ++ show xs'
           ) (Just t' == t'' && xs' <= xs)
      


prop_fromNodeToLeafAfter :: NTTT -> Property
prop_fromNodeToLeafAfter (N n) = forAll (pointInside (subtreeRegion $ snd n)) $ \p -> do
   let after = fromLeafToLeafAfter p n
       afterRegion = nodeRegion after
   whenFail (do putStrLn $ "after = " ++ show after
                putStrLn $ "after reg = " ++ show afterRegion
            ) 
     (regionStart afterRegion >= p)

prop_fromNodeToFinal :: NTTT -> Property
prop_fromNodeToFinal  (N t) = forAll (regionInside (subtreeRegion $ snd t)) $ \r -> do
   let final@(finalPath, finalSubtree) = fromNodeToFinal r t
       finalRegion = subtreeRegion finalSubtree
   whenFail (do putStrLn $ "final = " ++ show final
                putStrLn $ "final reg = " ++ show finalRegion
                putStrLn $ "leaf after = " ++ show (fromLeafToLeafAfter (regionEnd r) t)
            ) $ do
     r `includedRegion` finalRegion
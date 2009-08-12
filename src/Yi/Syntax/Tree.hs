{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables #-}
{- Copyright JP Bernardy 2008 -}

-- | Generic syntax tree handling functions
module Yi.Syntax.Tree (IsTree(..), SubTree(..), toksAfter, allToks, tokAtOrBefore, toksInRegion,
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

fromNodeToFinal :: IsTree tree => Region -> Node (tree (Tok t)) -> Node (tree (Tok t))
fromNodeToFinal r (xs,root) = (xs',leaf)
    where n@(xs',_) = fromLeafToLeafAfter (regionEnd r) (xs,root)
          (_,leaf) = fromLeafAfterToFinal (regionStart r) n

-- | Return the first element that matches the predicate, or the last of the list
-- if none matches.
firstThat p [] = error "firstThat: empty list"
firstThat p [x] = x
firstThat p (x:xs) = if p x then x else firstThat p xs

-- | Given a path to a node, return a path+node which 
-- node that encompasses the given node + a point before it.
fromLeafAfterToFinal :: IsTree tree => Point -> Node (tree (Tok t)) -> Node (tree (Tok t))
fromLeafAfterToFinal p n = 
    -- trace ("reg = " ++ show (fmap (subtreeRegion . snd) nsPth)) $ 
      firstThat (\(_,s) -> getFirstOffset' s <= p) ns
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
    result
    where xs' = fst $ if leafBeforeP
                      then firstThat (\(_,s) -> getFirstOffset' s >= p) $ allLeavesRelative afterChild n
                      else firstThat (\(_,s) -> getFirstOffset' s <= p) $ allLeavesRelative beforeChild n
          (xsValid,leaf) = wkDown (xs,root)
          leafBeforeP = getFirstOffset' leaf <= p
          n = (xsValid,root)
          result = (xs',root)
           

-- allLeavesAfter :: IsTree tree => Node (tree t) -> [(Path, tree t)]

allLeavesRelative select = allLeavesRelative' select . reverse . nodesAndChildIndex


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
          
nodesOnPath :: IsTree tree => Node (tree t) -> [Node (tree t)]
nodesOnPath ([],t) = [([],t)]
nodesOnPath (x:xs,t) = ([],t) : case index x (subtrees t) of
                           Nothing -> []
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

getFirstTok, getLastTok :: (SubTree a) => a -> Maybe (Element a)
getFirstTok = getFirst . foldMapToks (First . Just) 
getLastTok = getLast . foldMapToks (Last . Just) 

-- | Return the last token of a subtree.
getLastElement :: Foldable t => t a -> Maybe a
getLastElement tree = getLast $ foldMap (Last . Just) tree


getLastOffset :: (Element a ~ Tok t, SubTree a) => a -> Point
getLastOffset = maybe 0 tokEnd . getLastTok


getFirstOffset :: (Element a ~ Tok t, SubTree a) => a -> Point
getFirstOffset = maybe 0 tokBegin . getFirstTok

getLastOffset' = maybe 0 tokEnd . getLastElement
getFirstOffset' = maybe 0 tokEnd . getFirstElement


subtreeRegion t = mkRegion (getFirstOffset' t) (getLastOffset' t) 

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


data Test a = Leaf a | Bin (Test a) (Test a) deriving (Show, Eq)

instance Foldable Test where
    foldMap f (Leaf x) = f x
    foldMap f (Bin l r) = foldMap f r <> foldMap f r

instance IsTree Test where
    subtrees (Leaf _) = []
    subtrees (Bin l r) = [l,r]


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
   let final@(finalPath, finalSubtree) = fromLeafAfterToFinal p n
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
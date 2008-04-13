--
-- Copyright (c) Krasimir Angelov 2008.
--
-- Generic zipper implementation for Data.Tree
--

module Data.Tree.Zipper
         ( TreeLoc(..), TreeCxt(..)

         -- * Moving Around
         , down
         , firstChild
         , lastChild
         , up
         , left
         , right
         , top
         , getTop

         -- * Node classification
         , isTop
         , isChild
         , isFirst
         , isLast
         , hasChildren

         -- * Tree-specific Mutation
         , insertLeft
         , insertRight
         , insertDown
         , insertDownAt
         , delete

         -- * Monad
         , modifyLabel
         , putLabel
         , getLabel
         ) where

import Control.Monad.State
import Data.Tree

data TreeCxt a = Top
               | Child { label  :: a,
                         parent :: TreeCxt a, -- parent's context
                         lefts  :: [Tree a],  -- siblings to the left
                         rights :: [Tree a]   -- siblings to the right
                       }
           deriving (Show, Eq)

data TreeLoc a = Loc { tree :: Tree a,
                       cxt  :: TreeCxt a
                     }
           deriving (Show, Eq)


-- Moving Around
--

-- | move down to the nth child
down :: Int -> State (TreeLoc a) a
down n = modify down' >> getLabel where
    down' (Loc (Node v cs) c) = let (t:ls,rs) = splitChildren [] cs (n+1)
                                    c' = Child { label  = v,
                                                 parent = c,
                                                 lefts  = ls,
                                                 rights = rs }
                                in Loc { tree = t, cxt = c' }

-- | move down to the first child
firstChild :: State (TreeLoc a) a
firstChild = modify down' >> getLabel where
    down' (Loc (Node _ []    ) _) = error "Cannot go down from an empty branch"
    down' (Loc (Node v (t:ts)) c) = let c' = Child { label  = v,
                                                     parent = c,
                                                     lefts  = [],
                                                     rights = ts }
                                    in Loc { tree = t, cxt = c' }

-- | move down to the last child
lastChild :: State (TreeLoc a) a
lastChild = modify down' >> getLabel where
    down' (Loc (Node v ts) c) =
      case reverse ts of
        []     -> error "Cannot go down from an empty branch"
        (t:ts) -> let c' = Child { label  = v,
                                   parent = c,
                                   lefts  = ts,
                                   rights = [] }
                  in Loc { tree = t, cxt = c' }

-- | move up
up :: State (TreeLoc a) a
up = modify up' >> getLabel where
    up' (Loc _ Top              ) = error "Cannot go up from the top node"
    up' (Loc t (Child v c ls rs)) = Loc { tree = Node v (combChildren ls t rs), cxt = c }


-- | move left a sibling
left :: State (TreeLoc a) a
left = modify left' >> getLabel where
    left' (Loc t Top              ) = error $ "Cannot move left in the root node"
    left' (Loc t (Child v c ls rs)) =
      case ls of
        []     -> error $ "Cannot move left from the first node"
        (l:ls) -> let c' = Child { label  = v,
                                   parent = c,
                                   lefts  = ls,
                                   rights = t : rs }
                  in Loc { tree = l, cxt = c' }

-- | move right a sibling
right :: State (TreeLoc a) a
right = modify right' >> getLabel where
    right' (Loc t Top              ) = error $ "Cannot move right in the root node"
    right' (Loc t (Child v c ls rs)) =
      case rs of
        []     -> error $ "Cannot move right from the last node"
        (r:rs) -> let c' = Child { label  = v,
                                   parent = c,
                                   lefts  = t:ls,
                                   rights = rs }
                  in Loc { tree = r, cxt = c' }

-- | move to the top node
top :: State (TreeLoc a) a
top = do
  b <- gets isChild
  if b then up >> top else getLabel

-- | get the Loc corresponding to the top of the tree
-- useful for when calling traverse.
-- e.g. (getTop t) `traverse` myPath
getTop :: Tree a -> TreeLoc a
getTop t = (Loc t Top)

-- Node classification
--

-- | is the top node
isTop :: TreeLoc a -> Bool
isTop loc = case loc of
              (Loc _ Top) -> True
              (Loc _ _  ) -> False

-- | is not the top node (i.e. the child of some other node)
isChild :: TreeLoc a -> Bool
isChild = not . isTop

-- | is the first node in its siblings list?
isFirst :: TreeLoc a -> Bool
isFirst loc = case loc of
                (Loc _ Top             ) -> True
                (Loc _ (Child _ _ [] _)) -> True
                (Loc _ _               ) -> False

-- | is the last node in its siblings list?
isLast :: TreeLoc a -> Bool
isLast loc = case loc of
               (Loc _ Top             ) -> True
               (Loc _ (Child _ _ _ [])) -> True
               (Loc _ _               ) -> False

-- | is there children
hasChildren :: TreeLoc a -> Bool
hasChildren = not . null . subForest . tree

-- Tree-specific Mutation
-- 

-- | insert a subtree to the left of the current node
insertLeft :: a -> State (TreeLoc a) ()
insertLeft v' = modify insertLeft' where
    insertLeft' (Loc _ Top) = error "Cannot insert left of the top node"
    insertLeft' (Loc t c  ) = let c' = Child { label  = label c,
                                               parent = parent c,
                                               rights = t : rights c,
                                               lefts  = lefts c }
                              in Loc { tree = Node v' [], cxt = c' }

-- | insert a subtree to the right of the current node
insertRight :: a -> State (TreeLoc a) ()
insertRight v' = modify insertRight' where
    insertRight' (Loc _ Top) = error "Cannot insert right of the top node"
    insertRight' (Loc t c  ) = let c' = Child { label  = label c,
                                                parent = parent c,
                                                rights = rights c,
                                                lefts  = t:lefts c }
                               in Loc { tree = Node v' [], cxt = c' }

-- | insert a subtree as the last child of the current node
insertDown :: a -> State (TreeLoc a) ()
insertDown v' = modify insertDown' where
    insertDown' (Loc (Node v cs) c) = let c' = Child { label  = v,
                                                       parent = c,
                                                       rights = [],
                                                       lefts  = reverse cs }
                                      in Loc { tree = Node v' [], cxt = c' }

-- | insert a subtree as the nth child of the current node
insertDownAt :: a -> Int -> State (TreeLoc a) ()
insertDownAt v' n = modify insertDA' where
    insertDA' (Loc (Node v cs) c) = let (ls,rs) = splitChildren [] cs n
                                        c' = Child { label  = v,
                                                     parent = c,
                                                     lefts  = ls,
                                                     rights = rs }
                                    in Loc { tree = Node v' [], cxt = c' }

-- | delete the current subtree. move right if possible, otherwise left if 
-- possible, otherwise fail
delete :: State (TreeLoc a) a
delete = modify del' >> getLabel where
    del' (Loc _ Top) = error "cannot delete the top node"
                     -- if no siblings, move up
    del' l@(Loc t c) | isLast l && isFirst l = 
                       let c' = Child { label  = label  $ parent c,
                                        parent = parent $ parent c,
                                        lefts  = lefts  $ parent c,
                                        rights = rights $ parent c }
                       in Loc { tree = Node (label c) [], cxt = c' }
                     -- if the last node, move left                        
                     | isLast l  = 
                       let c' = Child { label  = label  c,
                                        parent = parent c,
                                        lefts  = tail $ lefts c,
                                        rights = rights c }
                       in Loc { tree = head $ lefts c, cxt = c' }
                     -- otherwise, just move right
                     | otherwise = 
                       let c' = Child { label  = label  c,
                                        parent = parent c,
                                        lefts  = lefts  c,
                                        rights = tail $ rights c }
                       in Loc { tree = head $ rights c, cxt = c' }


-- Monad
-- 

-- | modify the label at the current node
modifyLabel :: (a -> a) -> State (TreeLoc a) ()
modifyLabel f = modify editStruct where
    editStruct (Loc (Node v ts) c) = Loc (Node (f v) ts) c

-- | put a new label at the current node
putLabel :: a -> State (TreeLoc a) ()
putLabel v = modify setStruct where
    setStruct (Loc (Node _ ts) c) = Loc (Node v ts) c

-- | get the current label
getLabel :: State (TreeLoc a) a
getLabel = gets (rootLabel . tree)


-- Utils
--

splitChildren acc xs     0 = (acc,xs)
splitChildren acc (x:xs) n = splitChildren (x:acc) xs $! n-1
splitChildren acc []     n = error "There aren't that many branches"

combChildren ls t rs = foldl (flip (:)) (t:rs) ls

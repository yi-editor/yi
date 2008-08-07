module Data.List.Diff (diff, Choice(..), Source(..)) where

-- A diff algorithm adapted from a edit-distance algorithm.
-- Not efficitent implementation...

import Data.List
import Data.Function

data Source = L | R | B
              deriving (Show, Eq)

data Choice a = Choice {choiceSource :: Source,
                        choiceContent :: a}
instance Show a => Show (Choice a) where
    show (Choice src a) = showSrc src ++ show a

showSrc :: Source -> String
showSrc L = "<"
showSrc B = " "
showSrc R = ">"

instance Functor Choice where
    fmap f (Choice c a) = Choice c (f a)

weight0 ::Source -> Int
weight0 B = 0
weight0 _ = 1

(+:) :: Choice t -> DList t -> DList t
(+:) h@(Choice src _) t = DCons (weight0 src + weight t) h t

weight :: DList a -> Int
weight DNil = 0
weight l@DCons {} = dweight l

data DList a 
    = DCons {dweight :: !Int, dhead :: (Choice a), dtail :: (DList a) }
    | DNil



toList DNil = []
toList (DCons _ h t) = h : toList t

type Diff a = [Choice a]

diff :: Eq a => [a] -> [a] -> Diff [a]
diff s t = simplify $ diffGen (==) const s t

diffGen :: (a -> a -> Bool) -- ^ compare two values
     -> (a -> a -> a)    -- ^ merge two values in the same equivalence class
     -> [a] -> [a] -> Diff a
diffGen (=?=) (=+=) a b = 
    tail $ reverse $ toList $
             last (if lab == 0 then mainDiag
	           else if lab > 0 then lowers !! (lab - 1)
		        else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag L R (error "diff: head element should not be forced") a b (head uppers) (DNil : head lowers)
	  uppers = eachDiag L R a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag R L b a (mainDiag : lowers) -- lower diagonals
	  eachDiag centerSrc edgeSrc a [] diags = []
	  eachDiag centerSrc edgeSrc a (bch0:bs) (lastDiag:diags) = oneDiag centerSrc edgeSrc bch0 a bs nextDiag lastDiag 
                                                                    : eachDiag centerSrc edgeSrc a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag centerSrc edgeSrc f a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach =?= bch then Choice B (ach =+= bch) +: nw 
                                                  else minBy weight (Choice edgeSrc bch +: head w) (Choice centerSrc ach +: head n)
		    firstelt = Choice edgeSrc f +: head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b

minBy f x y = if f x < f y then x else y

simplify :: Diff a -> Diff [a]
simplify = map simplOne . groupBy ((==) `on` choiceSource) 
           where simplOne l@((Choice src _):_) = Choice src (map choiceContent l)








-- maybe use package event-list instead.
module Data.DelayList (insert, decrease, DelayList) where

type DelayList a = [(Int, a)]

-- | Subtraction, but treat maxBound as infinity. i.e. maxBound -? x == maxBound
(-?) :: Int -> Int -> Int
x -? y | x == maxBound = x
       | otherwise = x - y

insert :: (Int, a) -> DelayList a -> DelayList a
insert (d, a) [] = [(d, a)]
insert (d, a) l@(h@(d', _):t)
       | d == d' = (d, a):t
       | d <  d' = (d, a) : decrease d l
       -- d >  d'
       | otherwise = h : insert (d -? d', a) t

decrease :: Int -> DelayList a -> DelayList a
decrease _ [] = []
decrease d l@((d',a):t)
    | d <= 0 = l
    | d < d' = (d' -? d, a):t
    -- d >= d'
    | otherwise = decrease (d - d') t

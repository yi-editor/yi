-- Copyright (C) 2008 JP Bernardy

module Yi.UI.Utils where
-- Utilities shared by various UIs

import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Prelude
import Prelude (Ordering(..))
import Yi.Window

-- | return index of Sol on line @n@ above current line
indexOfSolAbove :: Int -> BufferM Point
indexOfSolAbove n = savingPointB $ do
    gotoLnFrom (negate n)
    pointB

-- | Transform (scroll) the window so that the point is visible.
moveWinTosShowPoint :: FBuffer -> Window -> FBuffer
moveWinTosShowPoint b w = result
  where (_, result) = runBuffer w b $ 
            do ln <- curLn
               let gap = min (ln-1) (height w `div` 2)
               i <- indexOfSolAbove gap
               f <- fromMark <$> askMarks
               setMarkPointB f i
               return ()

-- | Turn a sequence of (from,style,to) strokes into a sequence
--   of picture points (from,style), taking special care to
--   ensure that the points are strictly increasing and introducing
--   padding segments where neccessary.
--   Precondition: Strokes are ordered and not overlapping.
strokePicture :: [(Point,(a -> a),Point)] -> [(Point,(a -> a))]
strokePicture [] = []
strokePicture wholeList@((leftMost,_,_):_) = helper leftMost wholeList
    where helper :: Point -> [(Point,(a -> a),Point)] -> [(Point,(a -> a))]
          helper prev [] = [(prev,id)]
          helper prev ((l,f,r):xs) 
              | prev < l  = (prev, id) : (l,f) : helper r xs
              | otherwise = (l,f) : helper r xs

-- | Paint the given stroke-picture on top of an existing picture
paintStrokes :: a -> [(Point,(a -> a))] -> [(Point,a)] -> [(Point,a)]
paintStrokes  _ [] ps = ps
paintStrokes s0 ss [] = [(p, f s0) | (p, f) <- ss]
paintStrokes s0 ls@((ps,f):ts) lp@((pp,s1):tp) =
  case ps `compare` pp of
    LT -> (ps, f s0):paintStrokes s0 ts lp
    EQ -> (ps, f s1):paintStrokes s1 ts tp
    GT -> (pp,   s1):paintStrokes s1 ls tp

paintPicture :: a -> [[(Point,(a -> a),Point)]] -> [(Point,a)]
paintPicture a = foldr (paintStrokes a . strokePicture) []


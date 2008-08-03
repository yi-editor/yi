-- Copyright (C) 2008 JP Bernardy

module Yi.UI.Utils where
-- Utilities shared by various UIs

import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Prelude
import Prelude ()
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

-- | @paintStrokes colorToTheRight strokes picture@: paint the strokes over a given picture.
-- Precondition: input list is sorted and strokes do not overlap.
-- Semantics of [(Point, a)] == from given on, point use the given color.
paintStrokes :: a -> [(Point,(a -> a),Point)] -> [(Point,a)] -> [(Point,a)]
paintStrokes _  []     rest = rest
paintStrokes s0 ss     [] = concat [[(l,s s0),(r,s0)] | (l,s,r) <- ss]
paintStrokes s0 ls@((l,s,r):ts) lp@((p,s'):tp) 
            | p < l  = (p,s') : paintStrokes s' ls tp
            | p <= r =          paintStrokes s' ls tp
            | r == p = (l,s s0) : (p,s') : paintStrokes s' ts tp 
            | otherwise {-r < p-}  = (l,s s0) : (r,s0) : paintStrokes s0 ts lp

paintPicture :: a -> [[(Point,(a -> a),Point)]] -> [(Point,a)]
paintPicture a = foldr (paintStrokes a) []

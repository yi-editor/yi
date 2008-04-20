-- Copyright (C) 2008 JP Bernardy

module Yi.UI.Utils where
-- Utilities shared by various UIs

import Yi.Buffer
import Yi.Window

-- | return index of Sol on line @n@ above current line
indexOfSolAbove :: Int -> BufferM Int
indexOfSolAbove n = savingPointB $ do
    gotoLnFrom (negate n)
    pointB

-- | Transform (scroll) the window so that the point is visible.
showPoint :: FBuffer -> Window -> Window 
showPoint b w = result
  where (result, _) = runBufferDummyWindow b $ 
            do ln <- curLn
               let gap = min (ln-1) (height w `div` 2)
               i <- indexOfSolAbove gap
               return w {tospnt = i}

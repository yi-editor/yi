{-# LANGUAGE DeriveDataTypeable,GeneralizedNewtypeDeriving #-}

module Yi.Hint (hintRotateLeft
               ,hintRotateRight
               ,hintRotateClear
               ,hintRotateGet
               ,hintRotateDo
               ) where

import Yi.Prelude (Initializable(..), getA, putA)
import Yi.Dynamic (YiVariable)
import Yi.Editor (dynA, withEditor)
import Yi.Keymap (YiM)
import Data.Typeable
import Data.Binary

-- rotate the hint messages with C-s C-r
newtype HintRotate = HintRotate Int
    deriving (Typeable, Binary)

instance Initializable HintRotate where initial = HintRotate 0
instance YiVariable HintRotate 

hintRotate' :: (Int -> Int) -> YiM ()
hintRotate' op = do
    HintRotate rotate <- withEditor $ getA dynA
    withEditor $ putA dynA (HintRotate $ op rotate)

hintRotateLeft :: YiM ()
hintRotateLeft  = hintRotate' (+ 1)

hintRotateRight :: YiM ()
hintRotateRight = hintRotate' (+ (-1))

hintRotateClear :: YiM ()
hintRotateClear = hintRotate' $ const 0

hintRotateGet :: YiM Int
hintRotateGet = do
    HintRotate rotate <- withEditor $ getA dynA
    return rotate

hintRotateDo :: [a] -> YiM [a]
hintRotateDo xs = do
    rotate <- hintRotateGet
    let rotate' = rotate `mod` length xs
        doRotate n xs = drop n xs ++ take n xs 
    return $ doRotate rotate' xs

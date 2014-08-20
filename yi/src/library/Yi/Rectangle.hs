-- Copyright (C) 2008 JP Bernardy
-- | emacs-style rectangle manipulation functions.
module Yi.Rectangle where

import Control.Applicative
import Control.Monad
import Data.List (sort, transpose)

import Yi.Buffer
import Yi.Editor
import Yi.String
import Text.Regex.TDFA

alignRegion :: String -> BufferM ()
alignRegion str = modifyRegionClever (alignText str) =<< unitWiseRegion Line =<< getSelectRegionB
    where regexSplit :: String -> String -> [String]
          regexSplit regex l = case l =~ regex of
              AllTextSubmatches (_:matches) -> matches
              _ -> error "regexSplit: text does not match"

          alignText :: String -> String -> String
          alignText regex text = unlines' ls'
            where ls, ls' :: [String]
                  ls = lines' text
                  columns, columns' :: [[String]]
                  columns = fmap (regexSplit regex) ls
                  columnsWidth :: [Int]
                  columnsWidth =  fmap (maximum . fmap length) $ transpose columns
                  columns' = fmap (zipWith padLeft columnsWidth) columns

                  ls' = fmap concat columns'



-- | Align each line of the region on the given regex.
-- Fails if it is not found in any line.
alignRegionOn :: String -> BufferM ()
alignRegionOn s = alignRegion ("^(.*)(" ++ s ++ ")(.*)")

-- | Get the selected region as a rectangle.
-- Returns the region extended to lines, plus the start and end columns of the rectangle.
getRectangle :: BufferM (Region, Int, Int)
getRectangle = do
    r <- getSelectRegionB
    extR <- unitWiseRegion Line r
    [lowCol,highCol] <- sort <$> mapM colOf [regionStart r, regionEnd r]
    return (extR, lowCol, highCol)

-- | Split a list at the boundaries given
multiSplit :: [Int] -> [a] -> [[a]]
multiSplit [] l = [l]
multiSplit (x:xs) l = left : multiSplit (fmap (subtract x) xs) right
    where (left,right) = splitAt x l

onRectangle :: (Int -> Int -> String -> String) -> BufferM ()
onRectangle f = do
    (reg, l, r) <- getRectangle
    modifyRegionB (mapLines (f l r)) reg

openRectangle :: BufferM ()
openRectangle = onRectangle openLine
    where openLine l r line = left ++ replicate (r-l) ' ' ++ right
              where (left,right) = splitAt l line

stringRectangle :: String -> BufferM ()
stringRectangle inserted = onRectangle stringLine
    where stringLine l r line = left ++ inserted ++ right
              where [left,_,right] = multiSplit [l,r] line

killRectangle :: EditorM ()
killRectangle = do
    cutted <- withBuffer0 $ do
        (reg, l, r) <- getRectangle
        text <- readRegionB reg
        let (cutted, rest) = unzip $ fmap cut $ lines' text
            cut line = let [left,mid,right] = multiSplit [l,r] line in (mid, left ++ right)
        replaceRegionB reg (unlines' rest)
        return cutted
    setRegE (unlines' cutted)

yankRectangle :: EditorM ()
yankRectangle = do
    text <- lines' <$> getRegE
    withBuffer0 $ forM_ text $ \t -> do
        savingPointB $ insertN t
        lineDown

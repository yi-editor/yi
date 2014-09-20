{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Rectangle
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- emacs-style rectangle manipulation functions.

module Yi.Rectangle where

import           Control.Applicative
import           Control.Monad
import           Data.List (intersperse)
import           Data.List (sort, transpose)
import           Data.Monoid
import qualified Data.Text as T
import           Text.Regex.TDFA
import           Yi.Buffer
import           Yi.Editor
import qualified Yi.Rope as R
import           Yi.String

alignRegion :: T.Text -> BufferM ()
alignRegion str = do
  s <- getSelectRegionB >>= unitWiseRegion Line
  modifyRegionClever (R.fromText . alignText str . R.toText) s
  where
    regexSplit :: String -> String -> [T.Text]
    regexSplit regex l = case l =~ regex of
        AllTextSubmatches (_:matches) -> T.pack <$> matches
        _ -> error "regexSplit: text does not match"

    alignText :: T.Text -> T.Text -> T.Text
    alignText regex text = unlines' ls'
      where ls, ls' :: [T.Text]
            ls = lines' text
            columns :: [[T.Text]]
            columns = regexSplit (T.unpack regex) <$> (T.unpack <$> ls)

            columnsWidth :: [Int]
            columnsWidth = fmap (maximum . fmap T.length) $ transpose columns

            columns' :: [[T.Text]]
            columns' = fmap (zipWith (`T.justifyLeft` ' ') columnsWidth) columns

            ls' = T.concat <$> columns'

-- | Align each line of the region on the given regex.
-- Fails if it is not found in any line.
alignRegionOn :: T.Text -> BufferM ()
alignRegionOn s = alignRegion $ "^(.*)(" `T.append` s `T.append` ")(.*)"

-- | Get the selected region as a rectangle.
-- Returns the region extended to lines, plus the start and end columns of the rectangle.
getRectangle :: BufferM (Region, Int, Int)
getRectangle = do
    r <- getSelectRegionB
    extR <- unitWiseRegion Line r
    [lowCol,highCol] <- sort <$> mapM colOf [regionStart r, regionEnd r]
    return (extR, lowCol, highCol)

-- | Split text at the boundaries given
multiSplit :: [Int] -> R.YiString -> [R.YiString]
multiSplit [] l = [l]
multiSplit (x:xs) l = left : multiSplit (fmap (subtract x) xs) right
    where (left, right) = R.splitAt x l

onRectangle :: (Int -> Int -> R.YiString -> R.YiString) -> BufferM ()
onRectangle f = do
  (reg, l, r) <- getRectangle
  modifyRegionB (mapLines (f l r)) reg

openRectangle :: BufferM ()
openRectangle = onRectangle openLine
  where
    openLine l r line =
      left <> R.fromText (T.replicate (r - l) (T.singleton ' ')) <> right
          where (left, right) = R.splitAt l line

stringRectangle :: R.YiString -> BufferM ()
stringRectangle inserted = onRectangle stringLine
  where stringLine l r line = left <> inserted <> right
          where [left,_,right] = multiSplit [l,r] line

killRectangle :: EditorM ()
killRectangle = do
  -- TODO: implement in yi-rope package smarter
  let unls x = mconcat $ intersperse (R.singleton '\n') x
  cutted <- withBuffer0 $ do
      (reg, l, r) <- getRectangle
      -- TODO: seems we could do this block over the rope fairly
      -- easily without converting
      text <- readRegionB reg
      let (cutted, rest) = unzip $ fmap cut $ R.lines' text

          cut :: R.YiString -> (R.YiString, R.YiString)
          cut line = let [left,mid,right] = multiSplit [l,r] line
                     in (mid, left <> right)
      replaceRegionB reg (unls rest)
      return cutted
  setRegE (unls cutted)

yankRectangle :: EditorM ()
yankRectangle = do
  text <- R.lines' <$> getRegE
  withBuffer0 $ forM_ text $ \t -> do
    savingPointB $ insertN t
    lineDown

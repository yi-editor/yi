{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
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

import           Control.Monad       (forM_)
import           Data.List           (sort, transpose)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T (Text, concat, justifyLeft, length)
import           Yi.Buffer
import           Yi.Editor           (EditorM, getRegE, setRegE, withCurrentBuffer)
import qualified Yi.Rope             as R
import           Yi.String           (lines', mapLines, unlines')

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
      left <> R.replicateChar (r - l) ' ' <> right
          where (left, right) = R.splitAt l line

stringRectangle :: R.YiString -> BufferM ()
stringRectangle inserted = onRectangle stringLine
  where stringLine l r line = left <> inserted <> right
          where [left,_,right] = multiSplit [l,r] line

killRectangle :: EditorM ()
killRectangle = do
  cutted <- withCurrentBuffer $ do
      (reg, l, r) <- getRectangle
      text <- readRegionB reg
      let (cutted, rest) = unzip $ fmap cut $ R.lines' text

          cut :: R.YiString -> (R.YiString, R.YiString)
          cut line = let [left,mid,right] = multiSplit [l,r] line
                     in (mid, left <> right)
      replaceRegionB reg (R.unlines rest)
      return cutted
  setRegE (R.unlines cutted)

yankRectangle :: EditorM ()
yankRectangle = do
  text <- R.lines' <$> getRegE
  withCurrentBuffer $ forM_ text $ \t -> do
    savingPointB $ insertN t
    lineDown

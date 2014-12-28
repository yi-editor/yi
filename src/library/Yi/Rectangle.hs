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

import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.List           (sort, transpose)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T (Text, concat, justifyLeft, length, pack, unpack)
import qualified Data.Text.ICU       as ICU (regex, find, unfold, group)
import           Yi.Buffer
import           Yi.Editor           (EditorM, getRegE, setRegE, withCurrentBuffer)
import qualified Yi.Rope             as R
import           Yi.String           (lines', mapLines, unlines')

alignRegion :: T.Text -> BufferM ()
alignRegion str = do
  s <- getSelectRegionB >>= unitWiseRegion Line
  modifyRegionB (R.fromText . alignText str . R.toText) s
  where
    regexSplit :: T.Text -> T.Text -> [T.Text]
    regexSplit pattern l = case ICU.find (ICU.regex [] pattern) l of
        Nothing -> error "regexSplit: text does not match"
        Just m  -> drop 1 $ ICU.unfold ICU.group m

    alignText :: T.Text -> T.Text -> T.Text
    alignText regex text = unlines' ls'
      where ls, ls' :: [T.Text]
            ls = lines' text
            columns :: [[T.Text]]
            columns = regexSplit regex <$> ls

            columnsWidth :: [Int]
            columnsWidth = fmap (maximum . fmap T.length) $ transpose columns

            columns' :: [[T.Text]]
            columns' = fmap (zipWith (`T.justifyLeft` ' ') columnsWidth) columns

            ls' = T.concat <$> columns'

-- | Align each line of the region on the given regex.
-- Fails if it is not found in any line.
alignRegionOn :: T.Text -> BufferM ()
alignRegionOn s = alignRegion $ "^(.*)(" <> s <> ")(.*)"

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

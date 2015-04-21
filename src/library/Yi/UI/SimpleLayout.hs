{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Yi.UI.SimpleLayout
    ( Rect (..)
    , Layout (..)
    , Point2D (..)
    , Size2D (..)
    , coordsOfCharacterB
    , layout
    , verticalOffsetsForWindows
    ) where

import           Prelude                        hiding (concatMap, mapM)

import           Control.Lens                   (use, (.~))
import           Control.Monad.State            (evalState, get, put)
import           Data.Foldable                  (find, toList)
import           Data.List                      (partition)
import qualified Data.List.PointedList.Circular as PL (PointedList)
import qualified Data.Map.Strict                as M (Map, fromList)
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T (uncons)
import           Data.Traversable               (mapM)
import           Yi.Buffer
import           Yi.Editor
import qualified Yi.Rope                        as R (take, toString, toText)
import           Yi.UI.Utils                    (arrangeItems)
import           Yi.Window

data Layout = Layout
    { tabbarRect :: !Rect
    , windowRects :: !(M.Map WindowRef Rect)
    , promptRect :: !Rect
    }

data Rect = Rect
    { offsetX :: !Int
    , offsetY :: !Int
    , sizeX :: !Int
    , sizeY :: !Int
    }

data Point2D = Point2D
    { pointCol :: !Int
    , pointRow :: !Int
    }

data Size2D = Size2D
    { sizeWidth :: !Int
    , sizeHeight :: !Int
    }

layout :: Int -> Int -> Editor -> (Editor, Layout)
layout colCount rowCount e =
    ( (windowsA .~ newWindows) e
    , Layout (Rect 0 0 colCount 1) winRects cmdRect
    )
    where
        (miniWs, ws) = partition isMini (toList (windows e))
        (cmd, _) = statusLineInfo e
        niceCmd = arrangeItems cmd colCount (maxStatusHeight e)
        cmdRect = Rect 0 (rowCount - cmdHeight - if null miniWs then 0 else 1) colCount cmdHeight
        cmdHeight = length niceCmd
        tabbarHeight = 1
        (heightQuot, heightRem) =
            quotRem
                (rowCount - tabbarHeight - if null miniWs then max 1 cmdHeight else 1 + cmdHeight)
                (length ws)
        heights = heightQuot + heightRem : repeat heightQuot
        offsets = scanl (+) 0 heights
        bigWindowsWithHeights =
            zipWith (\win h -> layoutWindow win e colCount h)
                    ws
                    heights
        miniWindowsWithHeights =
            fmap (\win -> layoutWindow win e colCount 1) miniWs
        newWindows =
            merge (miniWindowsWithHeights <> bigWindowsWithHeights) (windows e)
        winRects = M.fromList (bigWindowsWithRects <> miniWindowsWithRects)
        bigWindowsWithRects =
            zipWith (\w offset -> (wkey w, Rect 0 (offset + tabbarHeight) colCount (height w)))
                    bigWindowsWithHeights
                    offsets
        miniWindowsWithRects =
            map (\w -> (wkey w, Rect 0 (rowCount - 1) colCount 1))
                miniWindowsWithHeights
        merge :: [Window] -> PL.PointedList Window -> PL.PointedList Window
        merge updates =
            let replace (Window { wkey = k }) = fromJust (find ((== k) . wkey) updates)
            in fmap replace


layoutWindow :: Window -> Editor -> Int -> Int -> Window
layoutWindow win e w h = win
    { height = h
    , width = w
    , winRegion = mkRegion fromMarkPoint toMarkPoint
    , actualLines = dispLnCount
    }
    where
        b = findBufferWith (bufkey win) e
        evalBuffer action = fst (runBuffer win b action)

        -- Mini windows don't have a mode line.
        h' = h - if isMini win then 0 else 1

        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        Just (MarkSet fromM _ _) = evalBuffer (getMarks win)
        fromMarkPoint = if isMini win
                        then Point 0
                        else evalBuffer $ use $ markPointA fromM

        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        (toMarkPoint, wrapCount) = evalBuffer
            (lastVisiblePointAndWrapCountB (Size2D w h') fromMarkPoint)

        dispLnCount = h' - wrapCount


coordsOfCharacterB :: Size2D -> Point -> Point -> BufferM (Maybe Point2D)
coordsOfCharacterB _ topLeft char | topLeft > char = return Nothing
coordsOfCharacterB (Size2D w h) (Point topLeft) (Point char)
    | char - topLeft >= w * h = return Nothing
coordsOfCharacterB (Size2D w h) (Point topLeft) (Point char) = savingPointB $ do
    ts <- fmap tabSize indentSettingsB
    text <- fmap (R.toString . R.take (w * h)) (streamB Forward (Point topLeft))
    let go _  !y _ _ | y >= h = Nothing
        go !x !y 0 _ = Just (Point2D x y)
        go !x !y !n (c : d : t) =
            case (c, d, compare x wOffset) of
                ('\t',  _ , _) -> go (x + ts) y (n - 1) (d:t)
                ('\n',  _ , _) -> go 0 (y + 1) (n - 1) (d:t)
                (  _ ,'\n',EQ) -> go x y (n - 1) (d:t)
                (  _ ,  _ ,EQ) -> go (x - wOffset) (y + 1) (n - 1) (d:t)
                (  _ ,  _ , _) -> go (x + 1) y (n - 1) (d:t)
            where wOffset = w - 1
        go !x !y !n [c] =
            case (c, compare x wOffset) of
                ('\n', _) -> go 0 (y + 1) (n - 1) [c]
                (  _ , _) -> go (x + 1) y (n - 1) [c]
            where wOffset = w - 1
        go !x !y _ _ = Just (Point2D x y)
    return (go 0 0 (char - topLeft) text)

lastVisiblePointAndWrapCountB :: Size2D -> Point -> BufferM (Point, Int)
lastVisiblePointAndWrapCountB (Size2D w h) (Point topLeft) = savingPointB $ do
    ts <- fmap tabSize indentSettingsB
    text <- fmap (R.toText . R.take (w * h))
                 (streamB Forward (Point topLeft))
    let go !x !y !wc !n t | x > w = go (x - w) (y + 1) (wc + 1) n t
        go _  !y !wc !n _ | y >= h = (Point (n - 1), wc)
        go !x !y !wc !n (T.uncons -> Just (c, t)) =
            case c of
                '\t' -> go (x + ts) y wc (n + 1) t
                '\n' -> go 0 (y + 1) wc (n + 1) t
                _ -> go (x + 1) y wc (n + 1) t
        go _ _ !wc !n _ = (Point n, wc)
    return (go 0 0 0 topLeft text)

verticalOffsetsForWindows :: Int -> PL.PointedList Window -> PL.PointedList Int
verticalOffsetsForWindows startY ws =
    scanrT (+) startY (fmap (\w -> if isMini w then 0 else height w) ws)

-- As scanr, but generalized to a traversable (TODO)
scanrT :: (Int -> Int -> Int) -> Int -> PL.PointedList Int -> PL.PointedList Int
scanrT (+*+) k t = evalState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s

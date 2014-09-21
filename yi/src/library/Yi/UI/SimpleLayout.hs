module Yi.UI.SimpleLayout
    ( Rect (..)
    , Layout (..)
    , layout
    , verticalOffsetsForWindows
    ) where

import Prelude hiding (concatMap, mapM)

import Control.Lens
import Control.Monad.State (evalState, get, put)
import Data.Char
import Data.Foldable
import Data.List (partition)
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Traversable (mapM)

import Yi.Buffer
import Yi.Editor
import Yi.UI.Utils
import Yi.Window

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

layout :: Int -> Int -> Editor -> (Editor, Layout)
layout colCount rowCount e =
    ( ((windowsA .~ newWindows) e)
    , Layout (Rect 0 0 colCount 1) windowRects cmdRect
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
        Just newWindows = PL.fromList (miniWindowsWithHeights <> bigWindowsWithHeights)
        windowRects = M.fromList (bigWindowsWithRects <> miniWindowsWithRects)
        bigWindowsWithRects =
            zipWith (\w offset -> (wkey w, Rect 0 (offset + tabbarHeight) colCount (height w)))
                    bigWindowsWithHeights
                    offsets
        miniWindowsWithRects =
            map (\w -> (wkey w, Rect 0 (rowCount - 1) colCount 1))
                miniWindowsWithHeights

layoutWindow :: Window -> Editor -> Int -> Int -> Window
layoutWindow win e w h = win
    { height = h
    , winRegion = mkRegion fromMarkPoint toMarkPoint
    , actualLines = dispLnCount
    }
    where
        b = findBufferWith (bufkey win) e
        evalBuffer action = fst (runBuffer win b action)

        -- Mini windows don't have a mode line.
        h' = h - if isMini win then 0 else 1

        eofPoint = evalBuffer sizeB
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        Just (MarkSet fromM _ _) = evalBuffer (getMarks win)
        fromMarkPoint = if isMini win
                        then Point 0
                        else evalBuffer $ use $ markPointA fromM
        text = evalBuffer (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily

        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        tabWidth = tabSize $ evalBuffer indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        (toMarkPoint, dispLnCount) =
            layoutText h'
                       w
                       tabWidth
                       fromMarkPoint
                       ([(-1, c) | c <- prompt] ++ text ++ [(eofPoint, ' ')])
                             -- we always add one character which can be used to position the cursor at the end of file

layoutText
    :: Int    -- ^ The height of the part of the window we are in
    -> Int    -- ^ The width of the part of the window we are in
    -> Int    -- ^ The number of spaces to represent a tab character with.
    -> Point
    -> [(Point, Char)]  -- ^ The data to draw.
    -> (Point, Int)
layoutText h w tabWidth topPoint bufData
    | h == 0 || w == 0 = (topPoint, 0)
    | otherwise        = (bottomPoint, h - (length wrapped - h))
    where

    -- the number of lines that taking wrapping into account,
    -- we use this to calculate the number of lines displayed.
    wrapped = concatMap (wrapLine . concatMap expandGraphic) $ take h $ lines' bufData

    lns0 = take h wrapped
    bottomPoint = case lns0 of
                   [] -> topPoint
                   _ -> fst $ last $ last lns0

    -- | Cut a string in lines separated by a '\n' char. Note
    -- that we add a blank character where the \n was, so the
    -- cursor can be positioned there.
    lines' :: [(a, Char)] -> [[(a, Char)]]
    lines' [] = []
    lines' s =
        let (l, s') = break ((== '\n') . snd) s
        in case s' of
            []          -> [l]
            ((x, _):s'') -> (l++[(x, ' ')]) : lines' s''

    wrapLine :: [x] -> [[x]]
    wrapLine [] = []
    wrapLine l =
        let (x, rest) = splitAt w l
        in x : wrapLine rest

    expandGraphic (p, '\t') = replicate tabWidth (p, ' ')
    expandGraphic (p, c)
        | ord c < 32 = [(p, '^'),(p, chr (ord c + 64))]
        | otherwise = [(p, c)]


verticalOffsetsForWindows :: Int -> PL.PointedList Window -> PL.PointedList Int
verticalOffsetsForWindows startY windows =
    scanrT (+) startY (fmap (\w -> if isMini w then 0 else height w) windows)

-- As scanr, but generalized to a traversable (TODO)
scanrT :: (Int -> Int -> Int) -> Int -> PL.PointedList Int -> PL.PointedList Int
scanrT (+*+) k t = evalState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s

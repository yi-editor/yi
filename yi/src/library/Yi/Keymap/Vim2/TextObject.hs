module Yi.Keymap.Vim2.TextObject
  ( TextObject(..)
  , TextObjectDetectResult(..)
  , StyledRegion(..)
  , parseTextObject
  , textObjectRegionB
  ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Keymap.Vim2.EventUtils

newtype TextObject = TextObject String
    deriving Show

data TextObjectDetectResult = Success Int TextObject
                            | Partial
                            | Fail
    deriving Show

data StyledRegion = StyledRegion !RegionStyle !Region

-- text object grammar
-- textobject = @number? @operator @stylemod* @number? @stylemod* @motion
-- TODO: style modifiers
parseTextObject :: String -> TextObjectDetectResult
parseTextObject s = setCount count (parseCommand commandString)
    where (count, commandString) = splitCountedCommand s

setCount :: Int -> TextObjectDetectResult -> TextObjectDetectResult
setCount count (Success _ (TextObject b)) = Success count (TextObject b)
setCount _ r = r

parseCommand :: String -> TextObjectDetectResult
parseCommand "" = Partial
parseCommand "l" = Success 1 $ TextObject "l"
parseCommand "V" = Partial
parseCommand "Vl" = Success 1 $ TextObject "Vl"
parseCommand "w" = Success 1 $ TextObject "w"
parseCommand _ = Fail

-- | if 'move' does something besides moving cursor, bad things may happen
regionWithMoveB :: BufferM () -> BufferM Region
regionWithMoveB move = do
    p0 <- pointB
    move
    p1 <- pointB
    moveTo p0
    return $! mkRegion p0 p1

regionWithTwoMovesB :: BufferM () -> BufferM () -> BufferM Region
regionWithTwoMovesB move1 move2 = do
    start <- pointB
    move1
    p0 <- pointB
    move2
    p1 <- pointB
    moveTo start
    return $! mkRegion p0 p1

textObjectRegionB :: Int -> TextObject -> BufferM StyledRegion
textObjectRegionB count to = do
    result@(StyledRegion style reg) <- textObjectRegionB' count to
    -- from vim help:
    --
    -- 1. If the motion is exclusive and the end of the motion is in column 1, the
    --    end of the motion is moved to the end of the previous line and the motion
    --    becomes inclusive.  Example: "}" moves to the first line after a paragraph,
    --    but "d}" will not include that line.
    -- 						*exclusive-linewise*
    -- 2. If the motion is exclusive, the end of the motion is in column 1 and the
    --    start of the motion was at or before the first non-blank in the line, the
    --    motion becomes linewise.  Example: If a paragraph begins with some blanks
    --    and you do "d}" while standing on the first non-blank, all the lines of
    --    the paragraph are deleted, including the blanks.  If you do a put now, the
    --    deleted lines will be inserted below the cursor position.
    --
    -- TODO: case 2
    if style == Exclusive
    then do
        let end = regionEnd reg
        (_, endColumn) <- getLineAndColOfPoint end
        if endColumn == 0
        then return $ StyledRegion Inclusive $ reg { regionEnd = end -~ 1 }
        else return result
    else return result


textObjectRegionB' :: Int -> TextObject -> BufferM StyledRegion
textObjectRegionB' n (TextObject "l") = fmap (StyledRegion Inclusive) $ regionWithMoveB $
    moveXorEol n
textObjectRegionB' n (TextObject "w") = fmap (StyledRegion Exclusive) $ regionWithMoveB $
    replicateM_ n $ genMoveB unitViWord (Backward, InsideBound) Forward
textObjectRegionB' n (TextObject "Vl") = do
    reg <- regionWithTwoMovesB moveToSol (lineMoveRel (n-1) >> moveToEol)
    fmap (StyledRegion LineWise) $ inclusiveRegionB reg
textObjectRegionB' _n _to = return $ StyledRegion Inclusive emptyRegion

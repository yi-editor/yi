module Yi.Keymap.Vim2.TextObject
  ( TextObject(..)
  , TextObjectDetectResult(..)
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

textObjectRegionB :: Int -> TextObject -> BufferM Region
textObjectRegionB n (TextObject "l") = regionWithMoveB $
    moveXorEol n
textObjectRegionB n (TextObject "w") = regionWithMoveB $
    replicateM_ n $ genMoveB unitViWord (Backward, InsideBound) Forward
textObjectRegionB n (TextObject "Vl") = do
    reg <- regionWithTwoMovesB moveToSol (lineMoveRel (n-1) >> moveToEol)
    inclusiveRegionB reg
textObjectRegionB _n _to = return emptyRegion

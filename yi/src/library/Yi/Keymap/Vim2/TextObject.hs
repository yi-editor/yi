module Yi.Keymap.Vim2.TextObject
    ( TextObject(..)
    , TextObjectDetectResult(..)
    , parseTextObject
    , textObjectRegionB
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer

newtype TextObject = TextObject String

data TextObjectDetectResult = Success TextObject
                            | Partial
                            | Fail

parseTextObject :: String -> TextObjectDetectResult
parseTextObject "l" = Success $ TextObject "l"
parseTextObject "V" = Partial
parseTextObject "Vl" = Success $ TextObject "Vl"
parseTextObject "w" = Success $ TextObject "w"
parseTextObject _ = Fail

textObjectRegionB :: TextObject -> BufferM Region
textObjectRegionB (TextObject "l") = do
    p@(Point pos) <- pointB
    return $! mkRegion p (Point $ pos + 1)
textObjectRegionB (TextObject "w") = do
    p0 <- pointB
    genMoveB unitViWord (Backward, InsideBound) Forward
    p1 <- pointB
    moveTo p0
    return $! mkRegion p0 p1
textObjectRegionB (TextObject "Vl") = do
    reg <- regionOfB Line
    inclusiveRegionB reg
textObjectRegionB _to = return emptyRegion

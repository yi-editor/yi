module Yi.Keymap.Vim2.TextObject
  ( TextObject(..)
  , OperandDetectResult(..)
  , StyledRegion(..)
  , parseTextObject
  , regionOfTextObjectB
  , changeTextObjectCount
  ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Data.Maybe (isJust, fromJust)

import Yi.Buffer
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.StyledRegion

data TextObject = TextObject !Int !RegionStyle TextUnit

data OperandDetectResult = JustTextObject !TextObject
                         | JustMove !CountedMove
                         | Partial
                         | Fail

parseTextObject :: String -> OperandDetectResult
parseTextObject s = setOperandCount count (parseCommand commandString)
    where (count, commandString) = splitCountedCommand s

changeTextObjectCount :: Int -> TextObject -> TextObject
changeTextObjectCount count (TextObject _c s u) = TextObject count s u

setOperandCount :: Int -> OperandDetectResult -> OperandDetectResult
setOperandCount n (JustTextObject (TextObject _ s u)) = JustTextObject (TextObject n s u)
setOperandCount n (JustMove (CountedMove _ m)) = JustMove (CountedMove n m)
setOperandCount _ o = o

parseCommand :: String -> OperandDetectResult
parseCommand "" = Partial
parseCommand s | isJust (stringToMove s) = JustMove $ CountedMove 1 $ fromJust $ stringToMove s
parseCommand "V" = Partial
parseCommand "Vl" = JustTextObject $ TextObject 1 LineWise VLine
parseCommand _ = Fail

regionOfTextObjectB :: TextObject -> BufferM StyledRegion
regionOfTextObjectB = normalizeRegion <=< textObjectRegionB'

textObjectRegionB' :: TextObject -> BufferM StyledRegion
textObjectRegionB' (TextObject count style (GenUnit enclosing move)) =
    fmap (StyledRegion style) $ regionWithTwoMovesB
        (move Backward)
        (replicateM_ count $ move Forward)
textObjectRegionB' (TextObject count style unit) =
    fmap (StyledRegion style) $ regionWithTwoMovesB
        (maybeMoveB unit Backward)
        (replicateM_ count $ moveB unit Forward)

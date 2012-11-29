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

parseTextObject :: Char -> String -> OperandDetectResult
parseTextObject opChar s = setOperandCount count (parseCommand opChar commandString)
    where (count, commandString) = splitCountedCommand s

changeTextObjectCount :: Int -> TextObject -> TextObject
changeTextObjectCount count (TextObject _c s u) = TextObject count s u

setOperandCount :: Int -> OperandDetectResult -> OperandDetectResult
setOperandCount n (JustTextObject (TextObject _ s u)) = JustTextObject (TextObject n s u)
setOperandCount n (JustMove (CountedMove _ m)) = JustMove (CountedMove n m)
setOperandCount _ o = o

parseCommand :: Char ->  String -> OperandDetectResult
parseCommand _ "" = Partial
parseCommand _ "Vl" = JustTextObject $ TextObject 1 LineWise unitViLine
parseCommand o ('v':s) = parseCommand o s
parseCommand o ('V':s) = parseCommand o s
parseCommand o ('<':'C':'-':'v':'>':s) = parseCommand o s
parseCommand o s | [o] == s = parseCommand o "Vl"
parseCommand _ s | isJust (stringToMove s) = JustMove $ CountedMove 1 $ fromJust $ stringToMove s
parseCommand _ _ = Fail

regionOfTextObjectB :: TextObject -> BufferM StyledRegion
regionOfTextObjectB = normalizeRegion <=< textObjectRegionB'

textObjectRegionB' :: TextObject -> BufferM StyledRegion
textObjectRegionB' (TextObject count style unit) =
    fmap (StyledRegion style) $ regionWithTwoMovesB
        (maybeMoveB unit Backward)
        (replicateM_ count $ moveB unit Forward)

-- I've failed to understand why VLine doesn't work here. Oh well.
unitViLine :: TextUnit
unitViLine = GenUnit Document
    (\dir -> case dir of
        Forward -> do
            p0 <- pointB
            moveToEol
            p1 <- pointB
            when (p0 == p1) $ lineMoveRel 1 >> moveToEol
            rightB
            return True
        Backward -> moveToSol >> return True)

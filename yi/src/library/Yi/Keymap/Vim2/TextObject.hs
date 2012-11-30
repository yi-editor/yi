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

import Data.Char (isDigit)
import Data.List (isPrefixOf, drop)
import Data.Maybe (isJust, fromJust)

import Yi.Buffer
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.StyledRegion

data TextObject = TextObject !Int !RegionStyle TextUnit

data OperandDetectResult = JustTextObject !TextObject
                         | JustMove !CountedMove
                         | JustOperator !Int !RegionStyle -- ^ like dd and d2vd
                         | Partial
                         | Fail

parseTextObject :: Char -> String -> OperandDetectResult
parseTextObject opChar s = parseCommand count styleMod opChar commandString
    where (count, styleModString, commandString) = splitCountModifierCommand s
          styleMod = case styleModString of
                        "" -> id
                        "V" -> const LineWise
                        "<C-v>" -> const Block
                        "v" -> \style -> case style of
                                            Exclusive -> Inclusive
                                            _ -> Exclusive
                        _ -> error "Can't happen"


-- Parse event string that can go after operator
-- w -> (1, "", "w")
-- 2w -> (2, "", "w")
-- V2w -> (2, "V", "w")
-- v2V3<C-v>w -> (6, "<C-v>", "w")
-- vvvvvvvvvvvvvw -> (1, "v", "w")
splitCountModifierCommand :: String -> (Int, String, String)
splitCountModifierCommand = go "" 1 [""]
    where go ds count mods (h:t) | isDigit h = go (ds ++ [h]) count mods t
          go ds@(_:_) count mods s@(h:_) | not (isDigit h) = go [] (count * read ds) mods s
          go [] count mods (h:t) | h `elem` "vV" = go [] count ([h]:mods) t
          go [] count mods s | "<C-v>" `isPrefixOf` s = go [] count ("<C-v>":mods) (drop 5 s)
          go [] count mods s = (count, head mods, s)
          go ds count mods [] = (count * read ds, head mods, [])
          go (_:_) _ _ (_:_) = error "Can't happen because isDigit and not isDigit cover every case"

changeTextObjectCount :: Int -> TextObject -> TextObject
changeTextObjectCount count (TextObject _c s u) = TextObject count s u

parseCommand :: Int -> (RegionStyle -> RegionStyle) -> Char ->  String -> OperandDetectResult
parseCommand _ _ _ "" = Partial
-- parseCommand n sm o s | [o] == s = JustMove $ CountedMove n $ Move (sm LineWise) operatorLineMove
parseCommand n sm o s | [o] == s = JustOperator n (sm LineWise)
parseCommand n sm _ s | isJust (stringToMove s) = JustMove $ CountedMove n
                                              $ changeMoveStyle sm $ fromJust $ stringToMove s
parseCommand _ _ _ _ = Fail

regionOfTextObjectB :: TextObject -> BufferM StyledRegion
regionOfTextObjectB = normalizeRegion <=< textObjectRegionB'

textObjectRegionB' :: TextObject -> BufferM StyledRegion
textObjectRegionB' (TextObject count style unit) =
    fmap (StyledRegion style) $ regionWithTwoMovesB
        (maybeMoveB unit Backward)
        (replicateM_ count $ moveB unit Forward)

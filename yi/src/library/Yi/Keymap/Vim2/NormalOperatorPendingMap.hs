module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude

import Data.Char (isDigit)
import Data.List (isPrefixOf, drop)
import Data.Maybe (isJust, fromJust)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.TextObject
import Yi.Keymap.Vim2.Utils

defNormalOperatorPendingMap :: [VimBinding]
defNormalOperatorPendingMap = [textObject, escBinding]

textObject :: VimBinding
textObject = VimBindingE prereq action
    where
        prereq _ vs = case vsMode vs of
                            NormalOperatorPending _ -> True
                            _ -> False
        action e = do
            currentState <- getDynamic
            let partial = vsTextObjectAccumulator currentState
                operand = parseOperand opChar (partial ++ eventToString e)
                opChar = lastCharForOperator op
                (NormalOperatorPending op) = vsMode currentState
            case operand of
                Fail -> do
                    dropTextObjectAccumulatorE
                    resetCountE
                    switchModeE Normal
                    return Drop
                Partial -> do
                    accumulateTextObjectEventE e
                    return Continue
                _ -> do
                    count <- getCountE
                    dropTextObjectAccumulatorE
                    case operand of
                        JustTextObject cto@(CountedTextObject n _) -> do
                            normalizeCountE n
                            applyOperatorToTextObjectE op $ changeTextObjectCount (count * n) cto
                        JustMove (CountedMove n m) -> do
                            normalizeCountE n
                            region <- withBuffer0 $ regionOfMoveB $ CountedMove (count * n) m
                            applyOperatorToRegionE op region
                        JustOperator n style -> do
                            normalizeCountE n
                            normalizedCount <- getCountE
                            region <- withBuffer0 $ regionForOperatorLineB normalizedCount style
                            applyOperatorToRegionE op region
                    resetCountE
                    let (NormalOperatorPending op) = vsMode currentState
                    if op == OpChange
                    then do
                        switchModeE Insert
                        return Continue
                    else do
                        switchModeE Normal
                        return Finish

regionForOperatorLineB :: Int -> RegionStyle -> BufferM StyledRegion
regionForOperatorLineB n style = normalizeRegion =<< StyledRegion style <$> savingPointB (do
    current <- pointB
    if n == 1
    then do
        firstNonSpaceB
        p0 <- pointB
        return $! mkRegion p0 current
    else do
        discard $ lineMoveRel (n-2)
        moveToEol
        rightB
        firstNonSpaceB
        p1 <- pointB
        return $! mkRegion current p1)

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

data OperandParseResult = JustTextObject !CountedTextObject
                         | JustMove !CountedMove
                         | JustOperator !Int !RegionStyle -- ^ like dd and d2vd
                         | Partial
                         | Fail

parseOperand :: Char -> String -> OperandParseResult
parseOperand opChar s = parseCommand count styleMod opChar commandString
    where (count, styleModString, commandString) = splitCountModifierCommand s
          styleMod = case styleModString of
                        "" -> id
                        "V" -> const LineWise
                        "<C-v>" -> const Block
                        "v" -> \style -> case style of
                                            Exclusive -> Inclusive
                                            _ -> Exclusive
                        _ -> error "Can't happen"

parseCommand :: Int -> (RegionStyle -> RegionStyle) -> Char ->  String -> OperandParseResult
parseCommand _ _ _ "" = Partial
parseCommand _ _ _ "i" = Partial
parseCommand _ _ _ "a" = Partial
parseCommand _ _ _ "g" = Partial
parseCommand n sm o s | [o] == s = JustOperator n (sm LineWise)
-- TODO: refactor with Alternative
parseCommand n sm _ s | isJust (stringToMove s) = JustMove $ CountedMove n
                                                $ changeMoveStyle sm $ fromJust $ stringToMove s
parseCommand n sm _ s | isJust (stringToTextObject s) = JustTextObject $ CountedTextObject n
                                                      $ changeTextObjectStyle sm
                                                      $ fromJust $ stringToTextObject s
parseCommand _ _ _ _ = Fail


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

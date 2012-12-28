module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude

import Data.Char (isDigit)
import Data.List (isPrefixOf, drop)
import Data.Maybe (fromMaybe)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
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
                            NormalOperatorPending _ -> WholeMatch ()
                            _ -> NoMatch
        action evs = do
            currentState <- getDynamic
            let partial = vsTextObjectAccumulator currentState
                operand = parseOperand opChar (partial ++ evs)
                opChar = lastCharForOperator op
                (NormalOperatorPending op) = vsMode currentState
            case operand of
                NoOperand -> do
                    dropTextObjectAccumulatorE
                    resetCountE
                    switchModeE Normal
                    return Drop
                PartialOperand -> do
                    accumulateTextObjectEventE evs
                    return Continue
                _ -> do
                    count <- getCountE
                    dropTextObjectAccumulatorE
                    case operand of
                        JustTextObject cto@(CountedTextObject n _) -> do
                            normalizeCountE (Just n)
                            applyOperatorToTextObjectE op $ changeTextObjectCount (count * n) cto
                        JustMove (CountedMove n m) -> do
                            mcount <- getMaybeCountE
                            normalizeCountE n
                            region <- withBuffer0 $ regionOfMoveB $ CountedMove (maybeMult mcount n) m
                            applyOperatorToRegionE op region
                        JustOperator n style -> do
                            normalizeCountE (Just n)
                            normalizedCount <- getCountE
                            region <- withBuffer0 $ regionForOperatorLineB normalizedCount style
                            applyOperatorToRegionE op region
                        _ -> error "can't happen"
                    resetCountE
                    if op == OpChange
                    then do
                        switchModeE $ Insert 'c'
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
                         | PartialOperand
                         | NoOperand

parseOperand :: Char -> String -> OperandParseResult
parseOperand opChar s = parseCommand mcount styleMod opChar commandString
    where (mcount, styleModString, commandString) = splitCountModifierCommand s
          styleMod = case styleModString of
                        "" -> id
                        "V" -> const LineWise
                        "<C-v>" -> const Block
                        "v" -> \style -> case style of
                                            Exclusive -> Inclusive
                                            _ -> Exclusive
                        _ -> error "Can't happen"

parseCommand :: Maybe Int -> (RegionStyle -> RegionStyle) -> Char -> String -> OperandParseResult
parseCommand _ _ _ "" = PartialOperand
parseCommand _ _ _ "i" = PartialOperand
parseCommand _ _ _ "a" = PartialOperand
parseCommand _ _ _ "g" = PartialOperand
parseCommand n sm o s | [o] == s = JustOperator (fromMaybe 1 n) (sm LineWise)
parseCommand n sm _ s =
    case stringToMove s of
        WholeMatch m -> JustMove $ CountedMove n $ changeMoveStyle sm m
        PartialMatch -> PartialOperand
        NoMatch -> case stringToTextObject s of
            Just to -> JustTextObject $ CountedTextObject (fromMaybe 1 n)
                                      $ changeTextObjectStyle sm to
            Nothing -> NoOperand


-- Parse event string that can go after operator
-- w -> (Nothing, "", "w")
-- 2w -> (Just 2, "", "w")
-- V2w -> (Just 2, "V", "w")
-- v2V3<C-v>w -> (Just 6, "<C-v>", "w")
-- vvvvvvvvvvvvvw -> (Nothing, "v", "w")
splitCountModifierCommand :: String -> (Maybe Int, String, String)
splitCountModifierCommand = go "" Nothing [""]
    where go ds count mods (h:t) | isDigit h = go (ds ++ [h]) count mods t
          go ds@(_:_) count mods s@(h:_) | not (isDigit h) = go [] (maybeMult count (Just (read ds))) mods s
          go [] count mods (h:t) | h `elem` "vV" = go [] count ([h]:mods) t
          go [] count mods s | "<C-v>" `isPrefixOf` s = go [] count ("<C-v>":mods) (drop 5 s)
          go [] count mods s = (count, head mods, s)
          go ds count mods [] = (maybeMult count (Just (read ds)), head mods, [])
          go (_:_) _ _ (_:_) = error "Can't happen because isDigit and not isDigit cover every case"

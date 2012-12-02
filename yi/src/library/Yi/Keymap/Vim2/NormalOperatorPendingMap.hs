module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer
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
                operand = parseTextObject opChar (partial ++ eventToString e)
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
                        JustTextObject to@(TextObject n _ _) -> do
                            normalizeCountE n
                            applyOperatorToTextObjectE op $ changeTextObjectCount (count * n) to
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
        lineMoveRel (n-2)
        moveToEol
        rightB
        firstNonSpaceB
        p1 <- pointB
        return $! mkRegion current p1)

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

getOperatorE :: EditorM VimOperator
getOperatorE = do
    (NormalOperatorPending op) <- fmap vsMode getDynamic
    return op

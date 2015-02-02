{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.NormalOperatorPendingMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.NormalOperatorPendingMap
       (defNormalOperatorPendingMap) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (void, when)
import           Data.Char                  (isDigit)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T (init, last, pack, snoc, unpack)
import           Yi.Buffer.Adjusted         hiding (Insert)
import           Yi.Editor                  (getEditorDyn, withCurrentBuffer)
import           Yi.Keymap.Keys             (Key (KEsc), spec)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Motion
import           Yi.Keymap.Vim.Operator
import           Yi.Keymap.Vim.StateUtils
import           Yi.Keymap.Vim.StyledRegion (StyledRegion (..), normalizeRegion)
import           Yi.Keymap.Vim.TextObject
import           Yi.Keymap.Vim.Utils        (mkBindingE)

defNormalOperatorPendingMap :: [VimOperator] -> [VimBinding]
defNormalOperatorPendingMap operators = [textObject operators, escBinding]

textObject :: [VimOperator] -> VimBinding
textObject operators = VimBindingE f
  where
    f evs vs = case vsMode vs of
                        NormalOperatorPending _ -> WholeMatch $ action evs
                        _ -> NoMatch
    action (Ev evs) = do
        currentState <- getEditorDyn

        let partial = vsTextObjectAccumulator currentState
            opChar = Ev . T.pack $ lastCharForOperator op
            op = fromJust $ stringToOperator operators opname
            (NormalOperatorPending opname) = vsMode currentState

        -- vim treats cw as ce
        let evs' = if opname == Op "c" && T.last evs == 'w' &&
                       (case parseOperand opChar (evr evs) of
                           JustMove _ -> True
                           _ -> False)
                   then T.init evs `T.snoc` 'e'
                   else evs
            -- TODO: fix parseOperand to take EventString as second arg
            evr x = T.unpack . _unEv $ partial <> Ev x
            operand = parseOperand opChar (evr evs')

        case operand of
            NoOperand -> do
                dropTextObjectAccumulatorE
                resetCountE
                switchModeE Normal
                return Drop
            PartialOperand -> do
                accumulateTextObjectEventE (Ev evs)
                return Continue
            _ -> do
                count <- getCountE
                dropTextObjectAccumulatorE
                token <- case operand of
                    JustTextObject cto@(CountedTextObject n _) -> do
                        normalizeCountE (Just n)
                        operatorApplyToTextObjectE op 1 $
                            changeTextObjectCount (count * n) cto
                    JustMove (CountedMove n m) -> do
                        mcount <- getMaybeCountE
                        normalizeCountE n
                        region <- withCurrentBuffer $ regionOfMoveB $ CountedMove (maybeMult mcount n) m
                        operatorApplyToRegionE op 1 region
                    JustOperator n style -> do
                        normalizeCountE (Just n)
                        normalizedCount <- getCountE
                        region <- withCurrentBuffer $ regionForOperatorLineB normalizedCount style
                        curPoint <- withCurrentBuffer pointB
                        token <- operatorApplyToRegionE op 1 region
                        when (opname == Op "y") $
                            withCurrentBuffer $ moveTo curPoint
                        return token

                    _ -> error "can't happen"
                resetCountE
                return token

regionForOperatorLineB :: Int -> RegionStyle -> BufferM StyledRegion
regionForOperatorLineB n style = normalizeRegion =<< StyledRegion style <$> savingPointB (do
    current <- pointB
    if n == 1
    then do
        firstNonSpaceB
        p0 <- pointB
        return $! mkRegion p0 current
    else do
        void $ lineMoveRel (n-2)
        moveToEol
        rightB
        firstNonSpaceB
        p1 <- pointB
        return $! mkRegion current p1)

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

data OperandParseResult
    = JustTextObject !CountedTextObject
    | JustMove !CountedMove
    | JustOperator !Int !RegionStyle -- ^ like dd and d2vd
    | PartialOperand
    | NoOperand

parseOperand :: EventString -> String -> OperandParseResult
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

-- | TODO: should this String be EventString?
parseCommand :: Maybe Int -> (RegionStyle -> RegionStyle)
             -> EventString -> String -> OperandParseResult
parseCommand _ _ _ "" = PartialOperand
parseCommand _ _ _ "i" = PartialOperand
parseCommand _ _ _ "a" = PartialOperand
parseCommand _ _ _ "g" = PartialOperand
parseCommand n sm o s | o' == s = JustOperator (fromMaybe 1 n) (sm LineWise)
  where o' = T.unpack . _unEv $ o
parseCommand n sm _ "0" =
  let m = Move Exclusive False (const moveToSol)
  in JustMove (CountedMove n (changeMoveStyle sm m))
parseCommand n sm _ s = case stringToMove . Ev $ T.pack s of
  WholeMatch m -> JustMove $ CountedMove n $ changeMoveStyle sm m
  PartialMatch -> PartialOperand
  NoMatch -> case stringToTextObject s of
    Just to -> JustTextObject $ CountedTextObject (fromMaybe 1 n)
               $ changeTextObjectStyle sm to
    Nothing -> NoOperand

-- TODO: setup doctests
-- Parse event string that can go after operator
-- w -> (Nothing, "", "w")
-- 2w -> (Just 2, "", "w")
-- V2w -> (Just 2, "V", "w")
-- v2V3<C-v>w -> (Just 6, "<C-v>", "w")
-- vvvvvvvvvvvvvw -> (Nothing, "v", "w")
-- 0 -> (Nothing, "", "0")
-- V0 -> (Nothing, "V", "0")
splitCountModifierCommand :: String -> (Maybe Int, String, String)
splitCountModifierCommand = go "" Nothing [""]
    where go "" Nothing mods "0" = (Nothing, head mods, "0")
          go ds count mods (h:t) | isDigit h = go (ds <> [h]) count mods t
          go ds@(_:_) count mods s@(h:_) | not (isDigit h) = go [] (maybeMult count (Just (read ds))) mods s
          go [] count mods (h:t) | h `elem` ['v', 'V'] = go [] count ([h]:mods) t
          go [] count mods s | "<C-v>" `isPrefixOf` s = go [] count ("<C-v>":mods) (drop 5 s)
          go [] count mods s = (count, head mods, s)
          go ds count mods [] = (maybeMult count (Just (read ds)), head mods, [])
          go (_:_) _ _ (_:_) = error "Can't happen because isDigit and not isDigit cover every case"

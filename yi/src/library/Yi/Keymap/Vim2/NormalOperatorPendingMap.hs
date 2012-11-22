module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.TextObject
import Yi.Keymap.Vim2.Utils

defNormalOperatorPendingMap :: [VimBinding]
defNormalOperatorPendingMap = [lineShortCut, textObject, escBinding]

-- things like dd, yy or gUU
-- when user repeats the last char of operator sequence,
-- operator is applied to current line
lineShortCut :: VimBinding
lineShortCut = VimBindingE prereq action
    where prereq e vs = case vsMode vs of
                            NormalOperatorPending _ -> not (null (vsAccumulator vs))
                                                       && eventToString e == [last $ vsAccumulator vs]
                            _ -> False
          action _ = do
              op <- getOperatorE
              count <- getCountE
              withBuffer0 . replicateM_ count $ applyOperatorToTextObjectB op $ TextObject "Vl"
              return Finish

textObject :: VimBinding
textObject = VimBindingE prereq action
    where prereq _ vs = case vsMode vs of
                            NormalOperatorPending _ -> True
                            _ -> False
          action e = do
              partial <- fmap vsTextObjectAccumulator getDynamic
              case parseTextObject (partial ++ eventToString e) of
                  Fail -> do
                      dropTextObjectAccumulatorE
                      resetCountE
                      switchModeE Normal
                      return Drop
                  Partial -> do
                      accumulateTextObjectEventE e
                      return Continue
                  Success to -> do
                      op <- getOperatorE
                      count <- getCountE
                      -- TODO normalize count
                      --      2d3w -> 6dw
                      dropTextObjectAccumulatorE
                      withBuffer0 . replicateM_ count $ applyOperatorToTextObjectB op to
                      dropTextObjectAccumulatorE
                      resetCountE
                      switchModeE Normal
                      return Finish

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

getOperatorE :: EditorM VimOperator
getOperatorE = do
    (NormalOperatorPending op) <- fmap vsMode getDynamic
    return op

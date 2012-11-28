module Yi.Keymap.Vim2.NormalOperatorPendingMap
  ( defNormalOperatorPendingMap
  ) where

import Prelude ()
import Yi.Prelude

import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.TextObject
import Yi.Keymap.Vim2.Utils

defNormalOperatorPendingMap :: [VimBinding]
defNormalOperatorPendingMap = [textObject, escBinding]

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
                  Success n to -> do
                      op <- getOperatorE
                      count <- getCountE
                      modifyStateE $ \s -> s {
                              vsCount = Just $ count * n
                            , vsAccumulator = show (count * n)
                                    ++ snd (splitCountedCommand (normalizeCount (vsAccumulator s)))
                          }
                      dropTextObjectAccumulatorE
                      applyOperatorToTextObjectE op (count * n) to
                      resetCountE
                      switchModeE Normal
                      return Finish

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

getOperatorE :: EditorM VimOperator
getOperatorE = do
    (NormalOperatorPending op) <- fmap vsMode getDynamic
    return op

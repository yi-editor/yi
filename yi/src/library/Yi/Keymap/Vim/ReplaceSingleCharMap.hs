module Yi.Keymap.Vim.ReplaceSingleCharMap
    ( defReplaceSingleMap
    ) where

import Control.Monad

import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Utils
import Yi.Utils

defReplaceSingleMap :: [VimBinding]
defReplaceSingleMap = [escBinding, actualReplaceBinding]

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

actualReplaceBinding :: VimBinding
actualReplaceBinding = VimBindingE f
    where f evs s | ReplaceSingleChar == vsMode s = WholeMatch $ do
              currentState <- getDynamic
              let count = fromMaybe 1 $ vsCount currentState
              let replacer = case evs of
                              (c:[]) -> replaceCharB c
                              "<lt>" -> replaceCharB '<'
                              "<C-e>" -> replaceCharWithBelowB
                              "<C-y>" -> replaceCharWithAboveB
                              _ -> return ()
              withBuffer0 $ do
                  -- Is there more easy way to get distance to eol?
                  here <- pointB
                  moveToEol
                  eol <- pointB
                  moveTo here

                  let effectiveCount = min count (fromSize $ eol ~- here)

                  when (effectiveCount > 0) $ do
                      replicateM_ effectiveCount $ replacer >> rightB
                      leftB

              resetCountE
              switchModeE Normal
              return Finish
          f _ _ = NoMatch

module Yi.Keymap.Vim2.ReplaceSingleCharMap
    ( defReplaceSingleMap
    ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Data.Maybe (fromMaybe)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils

defReplaceSingleMap :: [VimBinding]
defReplaceSingleMap = [escBinding, actualReplaceBinding]

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

actualReplaceBinding :: VimBinding
actualReplaceBinding = VimBindingE prereq action
    where prereq _ s = ReplaceSingleChar == vsMode s
          action e = do
              currentState <- getDynamic
              let count = fromMaybe 1 $ vsCount currentState
              let replacer = case e of
                              (Event (KASCII c) []) -> replaceCharB c
                              (Event (KASCII 'e') [MCtrl]) -> replaceCharWithBelowB
                              (Event (KASCII 'y') [MCtrl]) -> replaceCharWithAboveB
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

replaceCharWithBelowB :: BufferM ()
replaceCharWithBelowB = replaceCharWithVerticalOffset 1

replaceCharWithAboveB :: BufferM ()
replaceCharWithAboveB = replaceCharWithVerticalOffset (-1)

replaceCharWithVerticalOffset :: Int -> BufferM ()
replaceCharWithVerticalOffset offset = do
    startingPoint <- pointB
    l0 <- curLn
    c0 <- curCol
    discard $ lineMoveRel offset
    l1 <- curLn
    c1 <- curCol
    if c0 == c1 && l0 + offset == l1
    then do c <- readB
            moveTo startingPoint
            replaceCharB c
    else moveTo startingPoint

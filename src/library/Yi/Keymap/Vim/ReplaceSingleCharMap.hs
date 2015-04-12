{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.ReplaceSingleCharMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.ReplaceSingleCharMap
    ( defReplaceSingleMap
    ) where

import           Control.Monad            (replicateM_, when)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T (unpack)
import           Yi.Buffer.Adjusted
import           Yi.Editor                (getEditorDyn, withCurrentBuffer)
import           Yi.Keymap.Keys           (Key (KEsc), spec)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.StateUtils (resetCount, resetCountE, switchMode, switchModeE)
import           Yi.Keymap.Vim.Utils      (mkBindingE)
import           Yi.Utils                 (SemiNum ((~-)))

defReplaceSingleMap :: [VimBinding]
defReplaceSingleMap = [escBinding, actualReplaceBinding]

escBinding :: VimBinding
escBinding = mkBindingE ReplaceSingleChar Drop (spec KEsc, return (), resetCount . switchMode Normal)

actualReplaceBinding :: VimBinding
actualReplaceBinding = VimBindingE (f . T.unpack . _unEv)
  where
    f evs s | ReplaceSingleChar == vsMode s = WholeMatch $ do
        currentState <- getEditorDyn
        let count = fromMaybe 1 $ vsCount currentState
        let replacer = case evs of
                        (c:[]) -> replaceCharB c
                        "<lt>" -> replaceCharB '<'
                        "<C-e>" -> replaceCharWithBelowB
                        "<C-y>" -> replaceCharWithAboveB
                        _ -> return ()
        withCurrentBuffer $ do
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

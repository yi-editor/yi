module Yi.Keymap.Vim2.NormalMap
  ( defNormalMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char

import Yi.Core (quitEditor)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Utils

mkDigitBinding :: Char -> (Event, EditorM (), VimState -> VimState)
mkDigitBinding c = (char c, return (), mutate)
    where mutate (VimState m Nothing) = VimState m (Just d)
          mutate (VimState m (Just count)) = VimState m (Just $ count * 10 + d)
          d = ord c - ord '0'

defNormalMap :: [VimBinding]
defNormalMap = [mkBindingY Normal (spec (KFun 10), quitEditor, id)] ++ pureBindings

pureBindings :: [VimBinding]
pureBindings = fmap (mkBindingE Normal) $
         [ (char 'h', vimMoveE VMLeft, resetCount)
         , (char 'l', vimMoveE VMRight, resetCount)
         , (char 'j', vimMoveE VMDown, resetCount)
         , (char 'k', vimMoveE VMUp, resetCount)
         , (char 'i', return (), switchMode Insert)
         , (spec KEsc, return (), resetCount)
         ]
         ++ fmap mkDigitBinding ['1' .. '9']

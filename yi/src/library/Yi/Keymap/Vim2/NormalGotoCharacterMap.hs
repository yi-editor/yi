module Yi.Keymap.Vim2.NormalGotoCharacterMap
    ( defNormalGotoCharacterMap
    ) where

import Prelude ()
import Yi.Prelude

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils

defNormalGotoCharacterMap :: [VimBinding]
defNormalGotoCharacterMap = [escBinding, charBinding]

escBinding :: VimBinding
escBinding = VimBindingE prereq action
    where prereq ev state | ev == Event KEsc [] =
                                case vsMode state of
                                    NormalGotoCharacter _ _ -> True
                                    _ -> False
                          | otherwise = False
          action _ = do
              switchModeE Normal
              resetCountE
              return Drop

charBinding :: VimBinding
charBinding = VimBindingE prereq action
    where prereq _ s = case vsMode s of
                           NormalGotoCharacter _ _ -> True
                           _ -> False
          action e = do
              case e of
                  (Event (KASCII c) []) -> do
                      (NormalGotoCharacter direction style) <- fmap vsMode getDynamic
                      count <- getCountE
                      withBuffer0 . replicateM_ count $ case (direction, style) of
                        (Forward, Inclusive) -> nextCInc c
                        (Forward, Exclusive) -> nextCInc c >> leftB
                        (Backward, Inclusive) -> prevCInc c
                        (Backward, Exclusive) -> prevCInc c >> rightB
                        (_, _) -> error "should never happen"
                  _ -> return ()
              resetCountE
              switchModeE Normal
              return Drop

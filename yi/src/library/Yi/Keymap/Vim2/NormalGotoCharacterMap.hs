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
                      let move = case (direction, style) of
                                    (Forward, Inclusive) -> nextCInLineInc c
                                    (Forward, Exclusive) -> nextCInLineExc c
                                    (Backward, Inclusive) -> prevCInLineInc c
                                    (Backward, Exclusive) -> prevCInLineExc c
                                    (_, _) -> error "should never happen"
                      withBuffer0 $ do
                          p0 <- pointB
                          replicateM_ (count - 1) $ do
                              move
                              when (style == Exclusive) $ moveB Character direction
                          p1 <- pointB
                          move
                          p2 <- pointB
                          when (p1 == p2) $ moveTo p0
                      let command = GotoCharCommand c direction style
                      modifyStateE $ \s -> s { vsLastGotoCharCommand = Just command}
                  _ -> return ()
              resetCountE
              switchModeE Normal
              return Drop

module Yi.Keymap.Vim2.NormalMap
  ( defNormalMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char
import Data.Maybe (fromMaybe)

import Yi.Buffer hiding (Insert)
import Yi.Core (quitEditor)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Eval
import Yi.Keymap.Vim2.Utils

mkDigitBinding :: Char -> (Event, EditorM (), VimState -> VimState)
mkDigitBinding c = (char c, return (), mutate)
    where mutate (VimState m Nothing a r) = VimState m (Just d) a r
          mutate (VimState m (Just count) a r) = VimState m (Just $ count * 10 + d) a r
          d = ord c - ord '0'

defNormalMap :: [VimBinding]
defNormalMap = [mkBindingY Normal (spec (KFun 10), quitEditor, id)] ++ pureBindings

pureBindings :: [VimBinding]
pureBindings =
    [zeroBinding] ++
    fmap (mkBindingE Normal) (
        [ (char 'h', vimMoveE (VMChar Backward), resetCount)
        , (char 'l', vimMoveE (VMChar Forward), resetCount)
        , (char 'j', vimMoveE (VMLine Forward), resetCount)
        , (char 'k', vimMoveE (VMLine Backward), resetCount)

        -- Word motions
        , (char 'w', vimMoveE (VMWordStart Forward), resetCount)
        , (char 'b', vimMoveE (VMWordStart Backward), resetCount)
        , (char 'e', vimMoveE (VMWordEnd Forward), resetCount)
        , (char 'W', vimMoveE (VMWORDStart Forward), resetCount)
        , (char 'B', vimMoveE (VMWORDStart Backward), resetCount)
        , (char 'E', vimMoveE (VMWORDEnd Forward), resetCount)

        -- Intraline stuff
        , (char '$', vimMoveE VMEOL, resetCount)
        , (char '^', vimMoveE VMNonEmptySOL, resetCount)

        -- Transition to insert mode
        , (char 'i', return (), switchMode Insert)
        , (char 'I', vimMoveE VMNonEmptySOL, switchMode Insert)
        , (char 'a', withBuffer0 rightB, switchMode Insert)
        , (char 'A', withBuffer0 moveToEol, switchMode Insert)
        , (char 'o', withBuffer0 $ do
                         moveToEol
                         insertB '\n'
            , switchMode Insert)
        , (char 'O', withBuffer0 $ do
                         moveToSol
                         insertB '\n'
                         leftB
            , switchMode Insert)
        , (spec KEsc, return (), resetCount)
        , (ctrlCh 'c', return (), resetCount)

        -- Changing
        , (char 'c', return (), switchMode Insert) -- TODO
        , (char 'C', return (), switchMode Insert) -- TODO

        -- Replacing
        , (char 'r', return (), switchMode ReplaceSingleChar)
        , (char 'R', return (), switchMode Replace)

        -- Deletion
        , (char 'x', return (), id) -- TODO
        , (char 'd', return (), id) -- TODO
        , (char 'D', return (), id) -- TODO

        -- Yanking
        , (char 'y', return (), id) -- TODO
        , (char 'Y', return (), id) -- TODO

        -- Pasting
        , (char 'p', return (), id) -- TODO
        , (char 'P', return (), id) -- TODO

        -- Search
        , (char '/', return (), id) -- TODO
        , (char '?', return (), id) -- TODO
        , (char '*', return (), id) -- TODO
        , (char '#', return (), id) -- TODO
        , (char 'n', return (), id) -- TODO
        , (char 'N', return (), id) -- TODO
        , (char 'f', return (), id) -- TODO
        , (char 'F', return (), id) -- TODO
        , (char 't', return (), id) -- TODO
        , (char 'T', return (), id) -- TODO

        -- Transition to visual
        , (char 'v', return (), id) -- TODO
        , (char 'V', return (), id) -- TODO
        , (ctrlCh 'v', return (), id) -- TODO

        -- Repeat
        , (char '.', do
                currentState <- getDynamic
                case vsRepeatableAction currentState of
                    Nothing -> return ()
                    Just (RepeatableAction prevCount actionString) -> do
                        let count = fromMaybe prevCount (vsCount currentState)
                        vimEval $ show count ++ actionString
            , id)
        , (char '&', return (), id) -- TODO

        -- Transition to ex
        , (char ':', return (), id) -- TODO

        -- Undo
        , (char 'u', return (), id) -- TODO
        , (char 'U', return (), id) -- TODO
        , (ctrlCh 'r', return (), id) -- TODO

        -- unsorted TODO
        , (char 'g', return (), id)
        , (char 's', return (), id)
        , (char '[', return (), id)
        , (char ']', return (), id)
        , (char '{', return (), id)
        , (char '}', return (), id)
        , (char '-', return (), id)
        , (char '+', return (), id)
        , (char '~', return (), id)
        , (spec KEnter, return (), id)
        ]
        ++ fmap mkDigitBinding ['1' .. '9']
    )

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq ev _ = ev == char '0'
          action _ = do
              currentState <- getDynamic
              case (vsCount currentState) of
                  Just c -> setDynamic $ currentState { vsCount = Just (10 * c) }
                  Nothing -> do
                      vimMoveE VMSOL
                      setDynamic $ resetCount currentState


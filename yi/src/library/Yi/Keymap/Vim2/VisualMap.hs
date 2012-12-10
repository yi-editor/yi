module Yi.Keymap.Vim2.VisualMap
  ( defVisualMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (isDigit, ord)
import Data.List (dropWhile)
import Data.Maybe (isJust)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion

defVisualMap :: [VimBinding]
defVisualMap = [escBinding, motionBinding] ++ operatorBindings ++ digitBindings ++ [todoBinding]

escBinding :: VimBinding
escBinding = VimBindingE prereq action
    where prereq e (VimState { vsMode = (Visual _) }) = e `elem` [spec KEsc, ctrlCh 'c']
          prereq _ _ = False
          action _ = do
              resetCountE
              clrStatus
              withBuffer0 $ do
                  setVisibleSelection False
                  putA regionStyleA Inclusive
              switchModeE Normal
              return Drop

digitBindings :: [VimBinding]
digitBindings = zeroBinding : fmap mkDigitBinding ['1' .. '9']

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq (Event (KASCII '0') []) (VimState { vsMode = (Visual _) }) = True
          prereq _ _ = False
          action _ = do
              currentState <- getDynamic
              case vsCount currentState of
                  Just c -> do
                      setDynamic $ currentState { vsCount = Just (10 * c) }
                      return Continue
                  Nothing -> do
                      withBuffer0 moveToSol
                      setDynamic $ resetCount currentState
                      return Continue

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = VimBindingE prereq action
    where prereq e (VimState { vsMode = (Visual _) }) | char c == e = True
          prereq _ _ = False
          action _ = do
              modifyStateE mutate
              return Continue
          mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
          mutate vs@(VimState {vsCount = Just count}) = vs { vsCount = Just $ count * 10 + d }
          d = ord c - ord '0'

motionBinding :: VimBinding
motionBinding = VimBindingE prereq action
    where prereq ev state@(VimState { vsMode = (Visual _) }) =
                                case ev of
                                    -- TODO: handle multichar motions
                                    Event (KASCII c) [] -> isJust (stringToMove [c])
                                    _ -> False
          prereq _ _ =  False
          action (Event (KASCII c) []) = do
              state <- getDynamic
              let (Just (Move _ move)) = stringToMove [c]
              count <- getMaybeCountE
              withBuffer0 $ move count >> leftOnEol
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (c == '$') $ setStickyEolE True
              when (c `elem` "jk" && vsStickyEol state) $ withBuffer0 $ moveToEol >> leftB
              when (c `notElem` "jk$") $ setStickyEolE False

              return Continue

-- TODO reduce duplication of operator list
operatorBindings :: [VimBinding]
operatorBindings = fmap mkOperatorBinding
    [ ('y', OpYank)
    , ('d', OpDelete)
    , ('D', OpDelete) -- TODO: make D delete to eol
    , ('x', OpDelete)
    , ('X', OpDelete)
    , ('c', OpChange)
    , ('u', OpLowerCase)
    , ('U', OpUpperCase)
    , ('~', OpSwitchCase)
    ]

regionOfSelectionB :: BufferM Region
regionOfSelectionB = savingPointB $ do
    start <- getSelectionMarkPointB
    stop <- pointB
    return $! mkRegion start stop

mkOperatorBinding :: (Char, VimOperator) -> VimBinding
mkOperatorBinding (x, op) = VimBindingE prereq action
    where prereq (Event (KASCII c) []) (VimState { vsMode = (Visual _) }) = x == c
          prereq _ _ = False
          action _ = do
              (Visual style) <- vsMode <$> getDynamic
              region <- withBuffer0 regionOfSelectionB
              applyOperatorToRegionE op $ StyledRegion style region
              resetCountE
              if op == OpChange
              then do
                  switchModeE Insert
                  return Continue
              else do
                  switchModeE Normal
                  return Finish

todoBinding :: VimBinding
todoBinding = VimBindingE prereq action
    where prereq _ (VimState { vsMode = (Visual _) }) = True
          prereq _ _ = False
          action e = do
              withBuffer0 $ insertN $ "<Event " ++ show e ++ " not handled in Visual>"
              return Drop

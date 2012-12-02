module Yi.Keymap.Vim2.VisualMap
  ( defVisualMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (isDigit, ord)
import Data.List (dropWhile)
import Data.Maybe (isJust)

import Yi.Buffer
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
                      return Drop

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = VimBindingE prereq action
    where prereq (Event (KASCII c) []) (VimState { vsMode = (Visual _) }) = True
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
                                    Event (KASCII c) [] -> isJust (stringToMove s)
                                        where s = dropWhile isDigit (vsAccumulator state) ++ [c]
                                    _ -> False
          prereq _ _ =  False
          action (Event (KASCII c) []) = do
              state <- getDynamic
              let s = dropWhile isDigit (vsAccumulator state) ++ [c]
                  (Just (Move _ move)) = stringToMove s
              count <- getCountE
              withBuffer0 $ move count >> leftOnEol
              resetCountE

              -- moving with j/k after $ sticks cursor to the right edge
              when (c == '$') $ setStickyEolE True
              when (c `elem` "jk" && vsStickyEol state) $ withBuffer0 $ moveToEol >> leftB
              when (c `notElem` "jk$") $ setStickyEolE False

              return Drop

-- TODO reduce duplication of operator list
operatorBindings :: [VimBinding]
operatorBindings = fmap mkOperatorBinding
    [ ('y', OpYank)
    , ('d', OpDelete)
    , ('c', OpChange)
    ]

regionOfSelectionB :: BufferM Region
regionOfSelectionB = savingPointB $ do
    regStyle <- getA regionStyleA
    start <- getSelectionMarkPointB
    stop <- pointB
    mkRegionOfStyleB start stop regStyle

mkOperatorBinding :: (Char, VimOperator) -> VimBinding
mkOperatorBinding (x, op) = VimBindingE prereq action
    where prereq (Event (KASCII c) []) (VimState { vsMode = (Visual _) }) = x == c
          prereq _ _ = False
          action _ = do
              region <- withBuffer0 regionOfSelectionB
              applyOperatorToRegionE op $ StyledRegion Inclusive region
              return Finish

todoBinding :: VimBinding
todoBinding = VimBindingE prereq action
    where prereq _ (VimState { vsMode = (Visual _) }) = True
          prereq _ _ = False
          action e = do
              withBuffer0 $ insertN $ "<Event " ++ show e ++ " not handled in Visual>"
              return Drop

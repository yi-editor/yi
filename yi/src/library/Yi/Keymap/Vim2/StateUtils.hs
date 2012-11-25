module Yi.Keymap.Vim2.StateUtils
  ( switchMode
  , switchModeE
  , resetCount
  , resetCountE
  , setCountE
  , modifyStateE
  , getCountE
  , accumulateEventE
  , accumulateTextObjectEventE
  , flushAccumulatorIntoRepeatableActionE
  , dropAccumulatorE
  , dropTextObjectAccumulatorE
  ) where

import Yi.Prelude
import Prelude ()

import Data.Maybe (fromMaybe)

import Yi.Editor
import Yi.Event
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils

switchMode :: VimMode -> VimState -> VimState
switchMode mode state = state { vsMode = mode }

switchModeE :: VimMode -> EditorM ()
switchModeE mode = modifyStateE $ switchMode mode

modifyStateE :: (VimState -> VimState) -> EditorM ()
modifyStateE f = do
    currentState <- getDynamic
    setDynamic $ f currentState

resetCount :: VimState -> VimState
resetCount s = s { vsCount = Nothing }

resetCountE :: EditorM ()
resetCountE = modifyStateE resetCount

getCountE :: EditorM Int
getCountE = do
    currentState <- getDynamic
    return $! fromMaybe 1 (vsCount currentState)

setCountE :: Int -> EditorM ()
setCountE n = modifyStateE $ \s -> s { vsCount = Just n }

accumulateEventE :: Event -> EditorM ()
accumulateEventE e = modifyStateE $
    \s -> s { vsAccumulator = vsAccumulator s ++ eventToString e }

accumulateTextObjectEventE :: Event -> EditorM ()
accumulateTextObjectEventE e = modifyStateE $
    \s -> s { vsTextObjectAccumulator = vsTextObjectAccumulator s ++ eventToString e }

flushAccumulatorIntoRepeatableActionE :: EditorM ()
flushAccumulatorIntoRepeatableActionE = do
    currentState <- getDynamic
    let repeatableAction = stringToRepeatableAction $ vsAccumulator currentState
    modifyStateE $ \s -> s { vsRepeatableAction = (Just repeatableAction) 
                           , vsAccumulator = []
                           }

dropAccumulatorE :: EditorM ()
dropAccumulatorE = modifyStateE $ \s -> s { vsAccumulator = [] }

dropTextObjectAccumulatorE :: EditorM ()
dropTextObjectAccumulatorE = modifyStateE $ \s -> s { vsTextObjectAccumulator = [] }


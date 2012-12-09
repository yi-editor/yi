module Yi.Keymap.Vim2.StateUtils
  ( switchMode
  , switchModeE
  , resetCount
  , resetCountE
  , setCountE
  , modifyStateE
  , getMaybeCountE
  , getCountE
  , accumulateEventE
  , accumulateTextObjectEventE
  , flushAccumulatorIntoRepeatableActionE
  , dropAccumulatorE
  , dropTextObjectAccumulatorE
  , setDefaultRegisterE
  , getDefaultRegisterE
  , normalizeCountE
  , setStickyEolE
  ) where

import Yi.Prelude
import Prelude ()

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Rope as R

import Yi.Buffer.Normal
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

getMaybeCountE :: EditorM (Maybe Int)
getMaybeCountE = do
    fmap vsCount getDynamic

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
    modifyStateE $ \s -> s { vsRepeatableAction = Just repeatableAction
                           , vsAccumulator = []
                           }

dropAccumulatorE :: EditorM ()
dropAccumulatorE = modifyStateE $ \s -> s { vsAccumulator = [] }

dropTextObjectAccumulatorE :: EditorM ()
dropTextObjectAccumulatorE = modifyStateE $ \s -> s { vsTextObjectAccumulator = [] }

getDefaultRegisterE :: EditorM (Maybe Register)
getDefaultRegisterE = fmap (HM.lookup '\0' . vsRegisterMap) getDynamic

setDefaultRegisterE :: RegionStyle -> R.Rope -> EditorM ()
setDefaultRegisterE style rope = do
    rmap <- fmap vsRegisterMap getDynamic
    let rmap' = HM.insert '\0' (Register style rope) rmap
    modifyStateE $ \state -> state { vsRegisterMap = rmap' }

normalizeCountE :: Int -> EditorM ()
normalizeCountE n = do
    count <- getCountE
    modifyStateE $ \s -> s {
                       vsCount = Just $ count * n
                     , vsAccumulator = show (count * n)
                           ++ snd (splitCountedCommand (normalizeCount (vsAccumulator s)))
                   }

setStickyEolE :: Bool -> EditorM ()
setStickyEolE b = modifyStateE $ \s -> s { vsStickyEol = b }

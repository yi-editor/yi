module Yi.Keymap.Vim.StateUtils
    ( switchMode
    , switchModeE
    , resetCount
    , resetCountE
    , setCountE
    , modifyStateE
    , getMaybeCountE
    , getCountE
    , accumulateEventE
    , accumulateBindingEventE
    , accumulateTextObjectEventE
    , flushAccumulatorIntoRepeatableActionE
    , dropAccumulatorE
    , dropBindingAccumulatorE
    , dropTextObjectAccumulatorE
    , setRegisterE
    , getRegisterE
    , normalizeCountE
    , setStickyEolE
    , maybeMult
    , updateModeIndicatorE
    , saveInsertEventStringE
    ) where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Rope as R

import Yi.Buffer.Normal
import Yi.Editor
import Yi.Event
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.EventUtils
import Yi.Style (defaultStyle)

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
getMaybeCountE = fmap vsCount getDynamic

getCountE :: EditorM Int
getCountE = do
    currentState <- getDynamic
    return $! fromMaybe 1 (vsCount currentState)

setCountE :: Int -> EditorM ()
setCountE n = modifyStateE $ \s -> s { vsCount = Just n }

accumulateBindingEventE :: Event -> EditorM ()
accumulateBindingEventE e = modifyStateE $
    \s -> s { vsBindingAccumulator = vsBindingAccumulator s ++ eventToString e }

accumulateEventE :: Event -> EditorM ()
accumulateEventE e = modifyStateE $
    \s -> s { vsAccumulator = vsAccumulator s ++ eventToString e }

accumulateTextObjectEventE :: EventString -> EditorM ()
accumulateTextObjectEventE evs = modifyStateE $
    \s -> s { vsTextObjectAccumulator = vsTextObjectAccumulator s ++ evs }

flushAccumulatorIntoRepeatableActionE :: EditorM ()
flushAccumulatorIntoRepeatableActionE = do
    currentState <- getDynamic
    let repeatableAction = stringToRepeatableAction $ vsAccumulator currentState
    modifyStateE $ \s -> s { vsRepeatableAction = Just repeatableAction
                           , vsAccumulator = []
                           }

dropAccumulatorE :: EditorM ()
dropAccumulatorE = modifyStateE $ \s -> s { vsAccumulator = [] }

dropBindingAccumulatorE :: EditorM ()
dropBindingAccumulatorE = modifyStateE $ \s -> s { vsBindingAccumulator = [] }

dropTextObjectAccumulatorE :: EditorM ()
dropTextObjectAccumulatorE = modifyStateE $ \s -> s { vsTextObjectAccumulator = [] }

getRegisterE :: RegisterName -> EditorM (Maybe Register)
getRegisterE name = fmap (HM.lookup name . vsRegisterMap) getDynamic

setRegisterE :: RegisterName -> RegionStyle -> R.Rope -> EditorM ()
setRegisterE name style rope = do
    rmap <- fmap vsRegisterMap getDynamic
    let rmap' = HM.insert name (Register style rope) rmap
    modifyStateE $ \state -> state { vsRegisterMap = rmap' }

normalizeCountE :: Maybe Int -> EditorM ()
normalizeCountE n = do
    mcount <- getMaybeCountE
    modifyStateE $ \s -> s {
                       vsCount = maybeMult mcount n
                     , vsAccumulator = show (fromMaybe 1 (maybeMult mcount n))
                           ++ snd (splitCountedCommand (normalizeCount (vsAccumulator s)))
                   }

maybeMult :: Num a => Maybe a -> Maybe a -> Maybe a
maybeMult (Just a) (Just b) = Just (a * b)
maybeMult Nothing  Nothing = Nothing
maybeMult a        Nothing = a
maybeMult Nothing  b       = b

setStickyEolE :: Bool -> EditorM ()
setStickyEolE b = modifyStateE $ \s -> s { vsStickyEol = b }

updateModeIndicatorE :: VimMode -> EditorM ()
updateModeIndicatorE prevMode = do
    currentState <- getDynamic
    let mode = vsMode currentState
        paste = vsPaste currentState
    when (mode /= prevMode) $ do
        let modeName = case mode of
                        Insert _ -> "INSERT" ++ if paste then " (paste) " else ""
                        InsertNormal -> "(insert)"
                        InsertVisual -> "(insert) VISUAL"
                        Replace -> "REPLACE"
                        Visual Block -> "VISUAL BLOCK"
                        Visual LineWise -> "VISUAL LINE"
                        Visual _ -> "VISUAL"
                        _ -> ""
            decoratedModeName = if null modeName then "" else "-- " ++ modeName ++ " --"
        setStatus ([decoratedModeName], defaultStyle)

saveInsertEventStringE :: EventString -> EditorM ()
saveInsertEventStringE evs =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ evs }

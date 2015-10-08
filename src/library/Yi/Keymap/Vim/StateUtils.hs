{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.StateUtils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

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
    , flushAccumulatorE
    , dropAccumulatorE
    , dropBindingAccumulatorE
    , dropTextObjectAccumulatorE
    , setRegisterE
    , getRegisterE
    , normalizeCountE
    , maybeMult
    , updateModeIndicatorE
    , saveInsertEventStringE
    , resetActiveRegisterE
    ) where

import           Control.Applicative      ((<$>))
import           Control.Monad            (when)
import qualified Data.HashMap.Strict      as HM (insert, lookup)
import           Data.Maybe               (fromMaybe, isJust)
import           Data.Monoid              (Monoid (mempty), (<>))
import qualified Data.Text                as T (null)
import           Yi.Buffer.Normal         (RegionStyle (Block, LineWise))
import           Yi.Editor                (EditorM, getEditorDyn, putEditorDyn, setStatus)
import           Yi.Event                 (Event)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.EventUtils
import           Yi.Rope                  (YiString)
import           Yi.String                (showT)
import           Yi.Style                 (defaultStyle)

switchMode :: VimMode -> VimState -> VimState
switchMode mode state = state { vsMode = mode }

switchModeE :: VimMode -> EditorM ()
switchModeE mode = modifyStateE $ switchMode mode

modifyStateE :: (VimState -> VimState) -> EditorM ()
modifyStateE f = do
    currentState <- getEditorDyn
    putEditorDyn $ f currentState

resetCount :: VimState -> VimState
resetCount s = s { vsCount = Nothing }

resetCountE :: EditorM ()
resetCountE = modifyStateE resetCount

getMaybeCountE :: EditorM (Maybe Int)
getMaybeCountE = fmap vsCount getEditorDyn

getCountE :: EditorM Int
getCountE = do
    currentState <- getEditorDyn
    return $! fromMaybe 1 (vsCount currentState)

setCountE :: Int -> EditorM ()
setCountE n = modifyStateE $ \s -> s { vsCount = Just n }

accumulateBindingEventE :: Event -> EditorM ()
accumulateBindingEventE e = modifyStateE $
    \s -> s { vsBindingAccumulator = vsBindingAccumulator s <> eventToEventString e }

accumulateEventE :: Event -> EditorM ()
accumulateEventE e = modifyStateE $
    \s -> s { vsAccumulator = vsAccumulator s <> eventToEventString e }

accumulateTextObjectEventE :: EventString -> EditorM ()
accumulateTextObjectEventE evs = modifyStateE $
    \s -> s { vsTextObjectAccumulator = vsTextObjectAccumulator s <> evs }

flushAccumulatorE :: EditorM ()
flushAccumulatorE = do
    accum <- vsAccumulator <$> getEditorDyn
    let repeatableAction = stringToRepeatableAction accum
    modifyStateE $ \s ->
        s { vsRepeatableAction = Just repeatableAction
          , vsAccumulator = mempty
          , vsCurrentMacroRecording = fmap (fmap (<> accum))
                                           (vsCurrentMacroRecording s)
          }

dropAccumulatorE :: EditorM ()
dropAccumulatorE =
    modifyStateE $ \s ->
        let accum = vsAccumulator s
        in s { vsAccumulator = mempty
             , vsCurrentMacroRecording = fmap (fmap (<> accum))
                                              (vsCurrentMacroRecording s)
             }

dropBindingAccumulatorE :: EditorM ()
dropBindingAccumulatorE =
  modifyStateE $ \s -> s { vsBindingAccumulator = mempty }

dropTextObjectAccumulatorE :: EditorM ()
dropTextObjectAccumulatorE =
  modifyStateE $ \s -> s { vsTextObjectAccumulator = mempty }

getRegisterE :: RegisterName -> EditorM (Maybe Register)
getRegisterE name = fmap (HM.lookup name . vsRegisterMap) getEditorDyn

setRegisterE :: RegisterName -> RegionStyle -> YiString -> EditorM ()
setRegisterE name style rope = do
    rmap <- fmap vsRegisterMap getEditorDyn
    let rmap' = HM.insert name (Register style rope) rmap
    modifyStateE $ \state -> state { vsRegisterMap = rmap' }

normalizeCountE :: Maybe Int -> EditorM ()
normalizeCountE n = do
    mcount <- getMaybeCountE
    modifyStateE $ \s -> s {
      vsCount = maybeMult mcount n
      , vsAccumulator = Ev (showT . fromMaybe 1 $ maybeMult mcount n)
          <> snd (splitCountedCommand . normalizeCount $ vsAccumulator s)
      }

maybeMult :: Num a => Maybe a -> Maybe a -> Maybe a
maybeMult (Just a) (Just b) = Just (a * b)
maybeMult Nothing  Nothing = Nothing
maybeMult a        Nothing = a
maybeMult Nothing  b       = b

updateModeIndicatorE :: VimState -> EditorM ()
updateModeIndicatorE prevState = do
  currentState <- getEditorDyn
  let mode     = vsMode currentState
      prevMode = vsMode prevState
      paste = vsPaste currentState
      isRecording   = isJust . vsCurrentMacroRecording $ currentState
      prevRecording = isJust . vsCurrentMacroRecording $ prevState

  when (mode /= prevMode || isRecording /= prevRecording) $ do
      let modeName = case mode of
            Insert _ -> "INSERT" <> if paste then " (paste) " else ""
            InsertNormal -> "(insert)"
            InsertVisual -> "(insert) VISUAL"
            Replace -> "REPLACE"
            Visual Block -> "VISUAL BLOCK"
            Visual LineWise -> "VISUAL LINE"
            Visual _ -> "VISUAL"
            _ -> ""

          decoratedModeName' = if T.null modeName
                              then mempty
                              else "-- " <> modeName <> " --"
          decoratedModeName = if isRecording
                              then decoratedModeName' <> "recording"
                              else decoratedModeName'

      setStatus ([decoratedModeName], defaultStyle)

saveInsertEventStringE :: EventString -> EditorM ()
saveInsertEventStringE evs = modifyStateE $ \s ->
  s { vsOngoingInsertEvents = vsOngoingInsertEvents s <> evs }

resetActiveRegisterE :: EditorM ()
resetActiveRegisterE = modifyStateE $ \s -> s { vsActiveRegister = '\0' }

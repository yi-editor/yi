module Yi.Keymap.Vim2.VisualMap
  ( defVisualMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (ord)
import Data.List (drop, group)
import Data.Prototype (extractValue)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Vim (exMode, defKeymap)
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils

defVisualMap :: [VimBinding]
defVisualMap = [escBinding, motionBinding, changeVisualStyleBinding]
            ++ operatorBindings ++ digitBindings ++ [replaceBinding, switchEdgeBinding]
            ++ [insertBinding, exBinding]

escBinding :: VimBinding
escBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) =
              matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              resetCountE
              clrStatus
              withBuffer0 $ do
                  setVisibleSelection False
                  putA regionStyleA Inclusive
              switchModeE Normal
              return Drop

exBinding :: VimBinding
exBinding = VimBindingE prereq action
    where prereq ":" (VimState { vsMode = (Visual _) }) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              exMode (extractValue defKeymap) ":'<,'>"
              switchModeE Normal
              return Finish

digitBindings :: [VimBinding]
digitBindings = zeroBinding : fmap mkDigitBinding ['1' .. '9']

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq "0" (VimState { vsMode = (Visual _) }) = WholeMatch ()
          prereq _ _ = NoMatch
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

changeVisualStyleBinding :: VimBinding
changeVisualStyleBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) | evs `elem` ["v", "V", "<C-v>"] = WholeMatch ()
          prereq _ _ = NoMatch
          action evs = do
              currentMode <- fmap vsMode getDynamic
              let newStyle = case evs of
                                 "v" -> Inclusive
                                 "V" -> LineWise
                                 "<C-v>" -> Block
                  newMode = Visual newStyle
              if newMode == currentMode
              then do
                  vbeAction escBinding "<Esc>"
              else do
                  modifyStateE $ \s -> s { vsMode = newMode }
                  withBuffer0 $ do
                      putA regionStyleA newStyle
                      putA rectangleSelectionA $ Block == newStyle
                      setVisibleSelection True
                      pointB >>= setSelectionMarkPointB
                  return Finish

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = VimBindingE prereq action
    where prereq (c':[]) (VimState { vsMode = (Visual _) }) = matchFromBool $ c == c'
          prereq _ _ = NoMatch
          action _ = do
              modifyStateE mutate
              return Continue
          mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
          mutate vs@(VimState {vsCount = Just count}) = vs { vsCount = Just $ count * 10 + d }
          d = ord c - ord '0'

motionBinding :: VimBinding
motionBinding = mkMotionBinding $ \m -> case m of
                                     Visual _ -> True
                                     _ -> False

-- TODO reduce duplication of operator list
operatorBindings :: [VimBinding]
operatorBindings = fmap mkOperatorBinding
    [ ("y", OpYank)
    , ("Y", OpYank)
    , ("d", OpDelete)
    , ("D", OpDelete) -- TODO: make D delete to eol
    , ("x", OpDelete)
    , ("X", OpDelete)
    , ("c", OpChange)
    , ("u", OpLowerCase)
    , ("U", OpUpperCase)
    , ("~", OpSwitchCase)
    , ("gu", OpLowerCase)
    , ("gU", OpUpperCase)
    , ("g~", OpSwitchCase)
    , (">", OpShiftRight)
    , ("<lt>", OpShiftLeft)
    ]

regionOfSelectionB :: BufferM Region
regionOfSelectionB = savingPointB $ do
    start <- getSelectionMarkPointB
    stop <- pointB
    return $! mkRegion start stop

mkOperatorBinding :: (String, VimOperator) -> VimBinding
mkOperatorBinding (s, op) = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) = evs `matchesString` s
          prereq _ _ = NoMatch
          action _ = do
              (Visual style) <- vsMode <$> getDynamic
              region <- withBuffer0 regionOfSelectionB
              applyOperatorToRegionE op $ StyledRegion style region
              resetCountE
              clrStatus
              withBuffer0 $ do
                  setVisibleSelection False
                  putA regionStyleA Inclusive
              if op == OpChange
              then do
                  switchModeE $ Insert 'c'
                  return Continue
              else do
                  switchModeE Normal
                  return Finish

replaceBinding :: VimBinding
replaceBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) =
              case evs of
                "r" -> PartialMatch
                ('r':_:[]) -> WholeMatch ()
                _ -> NoMatch
          prereq _ _ = NoMatch
          action (_:c:[]) = do
              (Visual style) <- vsMode <$> getDynamic
              region <- withBuffer0 regionOfSelectionB
              withBuffer0 $ transformCharactersInRegionB (StyledRegion style region)
                                (\x -> if x == '\n' then x else c)
              switchModeE Normal
              return Finish
          action _ = error "can't happen"

switchEdgeBinding :: VimBinding
switchEdgeBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) =
              matchFromBool $ evs `elem` group "oO"
          prereq _ _ = NoMatch
          action (c:[]) = do
              (Visual style) <- vsMode <$> getDynamic
              withBuffer0 $ do
                  here <- pointB
                  there <- getSelectionMarkPointB
                  (here', there') <- case (c, style) of
                                        ('O', Block) -> flipRectangleB here there
                                        (_, _) -> return (there, here)
                  moveTo here'
                  setSelectionMarkPointB there'
              return Continue
          action _ = error "can't happen"

insertBinding :: VimBinding
insertBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) =
              matchFromBool $ evs `elem` group "IA"
          prereq _ _ = NoMatch
          action evs = do
              (Visual style) <- vsMode <$> getDynamic
              region <- withBuffer0 regionOfSelectionB
              cursors <- withBuffer0 $ case evs of
                  "I" -> leftEdgesOfRegionB style region
                  "A" -> rightEdgesOfRegionB style region
                  _ -> error "can't happen"
              withBuffer0 $ moveTo $ head cursors
              modifyStateE $ \s -> s { vsSecondaryCursors = drop 1 cursors }
              switchModeE $ Insert (head evs)
              return Continue

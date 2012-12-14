module Yi.Keymap.Vim2.VisualMap
  ( defVisualMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Ord

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils

defVisualMap :: [VimBinding]
defVisualMap = [escBinding, motionBinding]
            ++ operatorBindings ++ digitBindings ++ [replaceBinding, switchEdgeBinding]

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

motionBinding = mkMotionBinding $ \m -> case m of
                                     Visual _ -> True
                                     _ -> False

-- TODO reduce duplication of operator list
operatorBindings :: [VimBinding]
operatorBindings = fmap mkOperatorBinding
    [ ("y", OpYank)
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

switchEdgeBinding :: VimBinding
switchEdgeBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) = matchFromBool $ evs `elem` ["o", "O"]
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


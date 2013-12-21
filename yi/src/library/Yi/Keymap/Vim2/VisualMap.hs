module Yi.Keymap.Vim2.VisualMap
  ( defVisualMap
  ) where

import Yi.Prelude
import Prelude ()

import Data.Char (ord)
import Data.List (drop, group, length, reverse)
import Data.Maybe (fromJust)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Operator
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils
import Yi.MiniBuffer

defVisualMap :: [VimOperator] -> [VimBinding]
defVisualMap operators =
    [escBinding, motionBinding, changeVisualStyleBinding, setMarkBinding]
    ++ [chooseRegisterBinding]
    ++ operatorBindings operators ++ digitBindings ++ [replaceBinding, switchEdgeBinding]
    ++ [insertBinding, exBinding, shiftDBinding]

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
              void $ spawnMinibufferE ":'<,'>" id
              switchModeE Ex
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

setMarkBinding :: VimBinding
setMarkBinding = VimBindingE prereq action
    where prereq "m" (VimState { vsMode = (Visual _) }) = PartialMatch
          prereq ('m':_:[]) (VimState { vsMode = (Visual _) }) = WholeMatch ()
          prereq _ _ = NoMatch
          action ('m':c:[]) = do
              withBuffer0 $ setNamedMarkHereB [c]
              return Continue
          action _ = error "Can't happen"

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
                                 _ -> error "Can't happen because of prereq, this just prevents warning"
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
motionBinding = mkMotionBinding Continue $
    \m -> case m of
        Visual _ -> True
        _ -> False

regionOfSelectionB :: BufferM Region
regionOfSelectionB = savingPointB $ do
    start <- getSelectionMarkPointB
    stop <- pointB
    return $! mkRegion start stop

operatorBindings :: [VimOperator] -> [VimBinding]
operatorBindings operators = fmap mkOperatorBinding $ operators ++ visualOperators
    where visualOperators = fmap synonymOp
                                  [ ("x", "d")
                                  , ("~", "g~")
                                  , ("Y", "y")
                                  , ("u", "gu")
                                  , ("U", "gU")
                                  ]
          synonymOp (newName, existingName) =
                    VimOperator newName . operatorApplyToRegionE . fromJust
                    . stringToOperator operators $ existingName

chooseRegisterBinding :: VimBinding
chooseRegisterBinding = mkChooseRegisterBinding $
    \s -> case s of
        (VimState { vsMode = (Visual _) }) -> True
        _ -> False

shiftDBinding :: VimBinding
shiftDBinding = VimBindingE prereq action
    where prereq "D" (VimState { vsMode = (Visual _) }) = WholeMatch ()
          prereq _ _ = NoMatch
          action _ = do
              (Visual style) <- vsMode <$> getDynamic
              reg <- withBuffer0 regionOfSelectionB
              case style of
                  Block -> withBuffer0 $ do
                      (start, lengths) <- shapeOfBlockRegionB reg
                      moveTo start
                      startCol <- curCol
                      forM_ (reverse [0 .. length lengths - 1]) $ \l -> do
                          moveTo start
                          void $ lineMoveRel l
                          whenM (fmap (== startCol) curCol) deleteToEol
                      leftOnEol
                  _ ->  do
                      reg' <- withBuffer0 $ convertRegionToStyleB reg LineWise
                      reg'' <- withBuffer0 $ mkRegionOfStyleB (regionStart reg')
                                                              (regionEnd reg' -~ Size 1)
                                                              Exclusive
                      void $ operatorApplyToRegionE opDelete 1 $ StyledRegion LineWise reg''
              resetCountE
              switchModeE Normal
              return Finish

mkOperatorBinding :: VimOperator -> VimBinding
mkOperatorBinding op = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Visual _) }) =
              evs `matchesString` (operatorName op)
          prereq _ _ = NoMatch
          action _ = do
              (Visual style) <- vsMode <$> getDynamic
              region <- withBuffer0 regionOfSelectionB
              count <- getCountE
              token <- operatorApplyToRegionE op count $ StyledRegion style region
              resetCountE
              clrStatus
              withBuffer0 $ do
                  setVisibleSelection False
                  putA regionStyleA Inclusive
              return token

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

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.VisualMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- I'm a module waiting for some kind soul to give me a commentary!

module Yi.Keymap.Vim.VisualMap ( defVisualMap ) where

import           Control.Applicative        ((<$), (<$>))
import           Control.Lens               ((.=))
import           Control.Monad              (forM_, void)
import           Data.Char                  (ord)
import           Data.List                  (group)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T (unpack)
import           Yi.Buffer.Adjusted         hiding (Insert)
import           Yi.Editor
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Operator     (VimOperator (..), opDelete, stringToOperator)
import           Yi.Keymap.Vim.StateUtils
import           Yi.Keymap.Vim.StyledRegion (StyledRegion (StyledRegion), transformCharactersInRegionB)
import           Yi.Keymap.Vim.Tag          (gotoTag)
import           Yi.Keymap.Vim.Utils        (matchFromBool, mkChooseRegisterBinding, mkMotionBinding)
import           Yi.MiniBuffer              (spawnMinibufferE)
import           Yi.Monad                   (whenM)
import qualified Yi.Rope                    as R (toText)
import           Yi.Tag                     (Tag (Tag))
import           Yi.Utils                   (SemiNum ((-~)))

defVisualMap :: [VimOperator] -> [VimBinding]
defVisualMap operators =
    [escBinding, motionBinding, changeVisualStyleBinding, setMarkBinding]
    ++ [chooseRegisterBinding]
    ++ operatorBindings operators ++ digitBindings ++ [replaceBinding, switchEdgeBinding]
    ++ [insertBinding, exBinding, shiftDBinding]
    ++ [tagJumpBinding]

escAction :: EditorM RepeatToken
escAction = do
    resetCountE
    clrStatus
    withCurrentBuffer $ do
        setVisibleSelection False
        putRegionStyle Inclusive
    switchModeE Normal
    return Drop

escBinding :: VimBinding
escBinding = VimBindingE f
    where f evs (VimState { vsMode = (Visual _) }) = escAction <$
              matchFromBool (evs `elem` ["<Esc>", "<C-c>"])
          f _ _ = NoMatch

exBinding :: VimBinding
exBinding = VimBindingE f
    where f ":" (VimState { vsMode = (Visual _) }) = WholeMatch $ do
              void $ spawnMinibufferE ":" id
              withCurrentBuffer $ writeN "'<,'>"
              switchModeE Ex
              return Finish
          f _ _ = NoMatch

digitBindings :: [VimBinding]
digitBindings = zeroBinding : fmap mkDigitBinding ['1' .. '9']

zeroBinding :: VimBinding
zeroBinding = VimBindingE f
    where f "0" (VimState { vsMode = (Visual _) }) = WholeMatch $ do
              currentState <- getEditorDyn
              case vsCount currentState of
                  Just c -> do
                      setCountE (10 * c)
                      return Continue
                  Nothing -> do
                      withCurrentBuffer moveToSol
                      resetCountE
                      withCurrentBuffer $ stickyEolA .= False
                      return Continue
          f _ _ = NoMatch

setMarkBinding :: VimBinding
setMarkBinding = VimBindingE (f . T.unpack . _unEv)
    where f "m" (VimState { vsMode = (Visual _) }) = PartialMatch
          f ('m':c:[]) (VimState { vsMode = (Visual _) }) = WholeMatch $ do
              withCurrentBuffer $ setNamedMarkHereB [c]
              return Continue
          f _ _ = NoMatch

changeVisualStyleBinding :: VimBinding
changeVisualStyleBinding = VimBindingE f
    where f evs (VimState { vsMode = (Visual _) })
            | evs `elem` ["v", "V", "<C-v>"]
            = WholeMatch $ do
                  currentMode <- fmap vsMode getEditorDyn
                  let newStyle = case evs of
                         "v" -> Inclusive
                         "V" -> LineWise
                         "<C-v>" -> Block
                         _ -> error "Just silencing false positive warning."
                      newMode = Visual newStyle
                  if newMode == currentMode
                  then escAction
                  else do
                      modifyStateE $ \s -> s { vsMode = newMode }
                      withCurrentBuffer $ do
                          putRegionStyle newStyle
                          rectangleSelectionA .= (Block == newStyle)
                          setVisibleSelection True
                      return Finish
          f _ _ = NoMatch

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = VimBindingE (f . T.unpack . _unEv)
    where f [c'] (VimState { vsMode = (Visual _) }) | c == c'
            = WholeMatch $ do
                  modifyStateE mutate
                  return Continue
          f _ _ = NoMatch
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
                                  , ("s", "c")
                                  , ("S", "c")
                                  , ("C", "c")
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
shiftDBinding = VimBindingE (f . T.unpack . _unEv)
    where f "D" (VimState { vsMode = (Visual _) }) = WholeMatch $ do
              (Visual style) <- vsMode <$> getEditorDyn
              reg <- withCurrentBuffer regionOfSelectionB
              case style of
                  Block -> withCurrentBuffer $ do
                      (start, lengths) <- shapeOfBlockRegionB reg
                      moveTo start
                      startCol <- curCol
                      forM_ (reverse [0 .. length lengths - 1]) $ \l -> do
                          moveTo start
                          void $ lineMoveRel l
                          whenM (fmap (== startCol) curCol) deleteToEol
                      leftOnEol
                  _ ->  do
                      reg' <- withCurrentBuffer $ convertRegionToStyleB reg LineWise
                      reg'' <- withCurrentBuffer $ mkRegionOfStyleB (regionStart reg')
                                                              (regionEnd reg' -~ Size 1)
                                                              Exclusive
                      void $ operatorApplyToRegionE opDelete 1 $ StyledRegion LineWise reg''
              resetCountE
              switchModeE Normal
              return Finish
          f _ _ = NoMatch

mkOperatorBinding :: VimOperator -> VimBinding
mkOperatorBinding op = VimBindingE f
  where
    f evs (VimState { vsMode = (Visual _) }) =
      action <$ evs `matchesString` Ev (_unOp $ operatorName op)
    f _ _ = NoMatch
    action = do
        (Visual style) <- vsMode <$> getEditorDyn
        region <- withCurrentBuffer regionOfSelectionB
        count <- getCountE
        token <- operatorApplyToRegionE op count $ StyledRegion style region
        resetCountE
        clrStatus
        withCurrentBuffer $ do
            setVisibleSelection False
            putRegionStyle Inclusive
        return token

replaceBinding :: VimBinding
replaceBinding = VimBindingE (f . T.unpack . _unEv)
    where f evs (VimState { vsMode = (Visual _) }) =
              case evs of
                "r" -> PartialMatch
                ('r':c:[]) -> WholeMatch $ do
                    (Visual style) <- vsMode <$> getEditorDyn
                    region <- withCurrentBuffer regionOfSelectionB
                    withCurrentBuffer $ transformCharactersInRegionB (StyledRegion style region)
                                      (\x -> if x == '\n' then x else c)
                    switchModeE Normal
                    return Finish
                _ -> NoMatch
          f _ _ = NoMatch

switchEdgeBinding :: VimBinding
switchEdgeBinding = VimBindingE (f . T.unpack . _unEv)
    where f [c] (VimState { vsMode = (Visual _) }) | c `elem` ['o', 'O']
              = WholeMatch $ do
                  (Visual style) <- vsMode <$> getEditorDyn
                  withCurrentBuffer $ do
                      here <- pointB
                      there <- getSelectionMarkPointB
                      (here', there') <- case (c, style) of
                                            ('O', Block) -> flipRectangleB here there
                                            (_, _) -> return (there, here)
                      moveTo here'
                      setSelectionMarkPointB there'
                  return Continue
          f _ _ = NoMatch

insertBinding :: VimBinding
insertBinding = VimBindingE (f . T.unpack . _unEv)
    where f evs (VimState { vsMode = (Visual _) }) | evs `elem` group "IA"
            = WholeMatch $ do
                  (Visual style) <- vsMode <$> getEditorDyn
                  region <- withCurrentBuffer regionOfSelectionB
                  cursors <- withCurrentBuffer $ case evs of
                      "I" -> leftEdgesOfRegionB style region
                      "A" -> rightEdgesOfRegionB style region
                      _ -> error "Just silencing ghc's false positive warning."
                  case cursors of
                      (mainCursor : _) -> withCurrentBuffer (moveTo mainCursor)
                  modifyStateE $ \s -> s { vsSecondaryCursors = drop 1 cursors }
                  switchModeE $ Insert (head evs)
                  return Continue
          f _ _ = NoMatch

tagJumpBinding :: VimBinding
tagJumpBinding = VimBindingY (f . T.unpack . _unEv)
    where f "<C-]>" (VimState { vsMode = (Visual _) })
            = WholeMatch $ do 
                 tag <- Tag . R.toText <$> withCurrentBuffer
                            (regionOfSelectionB >>= readRegionB)
                 withEditor $ switchModeE Normal
                 gotoTag tag 0 Nothing
                 return Finish
          f _ _ = NoMatch

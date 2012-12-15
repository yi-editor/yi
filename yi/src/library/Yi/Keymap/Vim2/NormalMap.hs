module Yi.Keymap.Vim2.NormalMap
  ( defNormalMap
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Prototype (extractValue)
import qualified Data.Rope as R

import Yi.Buffer hiding (Insert)
import Yi.Core (quitEditor)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim (exMode, defKeymap)
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Eval
import Yi.Keymap.Vim2.OperatorUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = mkBindingE Normal Continue (char c, return (), mutate)
    where mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
          mutate vs@(VimState {vsCount = Just count}) = vs { vsCount = Just $ count * 10 + d }
          d = ord c - ord '0'

defNormalMap :: [VimBinding]
defNormalMap = mkBindingY Normal (spec (KFun 10), quitEditor, id) : pureBindings

pureBindings :: [VimBinding]
pureBindings =
    [zeroBinding, repeatBinding, motionBinding] ++
    fmap mkDigitBinding ['1' .. '9'] ++
    finishingBingings ++
    continuingBindings ++
    nonrepeatableBindings ++
    [tabTraversalBinding]

motionBinding = mkMotionBinding $ \m -> case m of
                                     Normal -> True
                                     _ -> False

zeroBinding :: VimBinding
zeroBinding = VimBindingE prereq action
    where prereq evs state = matchFromBool $ evs == "0" && (vsMode state == Normal)
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

repeatBinding :: VimBinding
repeatBinding = VimBindingE prereq action
    where prereq evs state = matchFromBool $ evs == "." && (vsMode state == Normal)
          action _ = do
                currentState <- getDynamic
                case vsRepeatableAction currentState of
                    Nothing -> return ()
                    Just (RepeatableAction prevCount actionString) -> do
                        let count = fromMaybe prevCount (vsCount currentState)
                        scheduleActionStringForEval $ show count ++ actionString
                        resetCountE
                return Drop

finishingBingings :: [VimBinding]
finishingBingings = fmap (mkBindingE Normal Finish)
    [ (char 'x', cutCharE Forward =<< getCountE, resetCount)
    , (char 'X', cutCharE Backward =<< getCountE, resetCount)

    -- Pasting
    , (char 'p', pasteAfter, id)
    , (char 'P', pasteBefore, id)

    -- Miscellaneous.
    -- TODO A repeat length longer than the current line toggles the final
    -- character too many times.
    , (char '~', flip replicateM_ (withBuffer0 $ switchCaseCharB >> leftOnEol) =<< getCountE, resetCount)
    ]

addNewLineIfNecessary :: Rope -> Rope
addNewLineIfNecessary rope = if lastChar == '\n'
                             then rope
                             else R.append rope (R.fromString "\n")
    where lastChar = head $ R.toString $ R.drop (R.length rope - 1) rope

pasteBefore :: EditorM ()
pasteBefore = do
    -- TODO: use count
    s <- getDefaultRegisterE
    case s of
        Nothing -> return ()
        Just (Register LineWise rope) -> withBuffer0 $ when (not $ R.null rope) $
            -- Beware of edge cases ahead
            insertRopeWithStyleB (addNewLineIfNecessary rope) LineWise
        Just (Register style rope) -> withBuffer0 $ pasteInclusiveB rope style

pasteAfter :: EditorM ()
pasteAfter = do
    -- TODO: use count
    register <- getDefaultRegisterE
    case register of
        Nothing -> return ()
        Just (Register LineWise rope) -> withBuffer0 $ do
            -- Beware of edge cases ahead
            moveToEol
            eof <- atEof
            when eof $ insertB '\n'
            rightB
            insertRopeWithStyleB (addNewLineIfNecessary rope) LineWise
            when eof $ savingPointB $ do
                newSize <- sizeB
                moveTo (newSize - 1)
                curChar <- readB
                when (curChar == '\n') $ deleteN 1
        Just (Register style rope) -> withBuffer0 $ do
            rightB
            pasteInclusiveB rope style

pasteInclusiveB :: Rope -> RegionStyle -> BufferM ()
pasteInclusiveB rope style = do
    p0 <- pointB
    insertRopeWithStyleB rope style
    if R.countNewLines rope == 0 && style `elem` [Exclusive, Inclusive]
    then leftB
    else moveTo p0

continuingBindings :: [VimBinding]
continuingBindings = fmap (mkBindingE Normal Continue)
    [ (char 'r', return (), switchMode ReplaceSingleChar)
    , (char 'd', return (), switchMode (NormalOperatorPending OpDelete))
    , (char 'y', return (), switchMode (NormalOperatorPending OpYank))
    , (char 'c', return (), switchMode (NormalOperatorPending OpChange))
    , (char '=', return (), switchMode (NormalOperatorPending OpReindent))
    , (char '>', return (), switchMode (NormalOperatorPending OpShiftRight))

    -- Transition to insert mode
    , (char 'i', return (), switchMode $ Insert 'i')
    , (char 'I', withBuffer0 firstNonSpaceB, switchMode $ Insert 'I')
    , (char 'a', withBuffer0 rightB, switchMode $ Insert 'a')
    , (char 'A', withBuffer0 moveToEol, switchMode $ Insert 'A')
    , (char 'o', withBuffer0 $ do
                     moveToEol
                     insertB '\n'
        , switchMode $ Insert 'o')
    , (char 'O', withBuffer0 $ do
                     moveToSol
                     insertB '\n'
                     leftB
        , switchMode $ Insert 'O')

    -- Transition to visual
    , (char 'v', enableVisualE Inclusive, resetCount . switchMode (Visual Inclusive))
    , (char 'V', enableVisualE LineWise, resetCount . switchMode (Visual LineWise))
    , (ctrlCh 'v', enableVisualE Block, resetCount . switchMode (Visual Block))
    ]
    ++ fmap (mkStringBindingE Normal Continue)
    [ ("gu", return (), switchMode (NormalOperatorPending OpLowerCase))
    , ("gU", return (), switchMode (NormalOperatorPending OpUpperCase))
    , ("g~", return (), switchMode (NormalOperatorPending OpSwitchCase))
    , ("gq", return (), switchMode (NormalOperatorPending OpFormat))
    , ("g?", return (), switchMode (NormalOperatorPending OpRot13))
    , ("<lt>", return (), switchMode (NormalOperatorPending OpShiftLeft))
    ]

nonrepeatableBindings :: [VimBinding]
nonrepeatableBindings = fmap (mkBindingE Normal Drop)
    [ (spec KEsc, return (), resetCount)
    , (ctrlCh 'c', return (), resetCount)

    -- Changing
    , (char 'C', do
        region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
        applyOperatorToRegionE OpDelete $ StyledRegion Exclusive region
        return (), switchMode $ Insert 'C')
    , (char 's', cutCharE Forward =<< getCountE, switchMode $ Insert 's')
    , (char 'S', do
        region <- withBuffer0 $ regionWithTwoMovesB firstNonSpaceB moveToEol
        applyOperatorToRegionE OpDelete $ StyledRegion Exclusive region
        , switchMode $ Insert 'S')

    -- Replacing
    , (char 'R', return (), switchMode Replace)

    -- Deletion
    , (char 'D', return (), id) -- TODO

    -- Yanking
    , (char 'y', return (), switchMode $ NormalOperatorPending OpYank) -- TODO
    , (char 'Y', do
        region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
        applyOperatorToRegionE OpYank $ StyledRegion Exclusive region
        , id) -- TODO

    -- Search
    , (char '/', return (), id) -- TODO
    , (char '?', return (), id) -- TODO
    , (char '*', return (), id) -- TODO
    , (char '#', return (), id) -- TODO
    , (char 'n', return (), id) -- TODO
    , (char 'N', return (), id) -- TODO
    , (char ';', repeatGotoCharE id, id)
    , (char ',', repeatGotoCharE reverseDir, id)

    -- Repeat
    , (char '&', return (), id) -- TODO

    -- Transition to ex
    , (char ':', switchToExE, id) -- TODO

    -- Undo
    , (char 'u', flip replicateM_ (withBuffer0 undoB) =<< getCountE, id)
    , (char 'U', flip replicateM_ (withBuffer0 undoB) =<< getCountE, id) -- TODO
    , (ctrlCh 'r', flip replicateM_ (withBuffer0 redoB) =<< getCountE, id)

    -- Indenting
    , (char '<', return (), id) -- TODO
    , (char '>', return (), id) -- TODO

    -- unsorted TODO
    , (char 'm', return (), id)
    , (char '-', return (), id)
    , (char '+', return (), id)
    , (char '"', return (), id)
    , (char 'q', return (), id)
    , (spec KEnter, return (), id)
    ]

switchToExE :: EditorM ()
switchToExE = exMode (extractValue defKeymap) ":"

repeatGotoCharE :: (Direction -> Direction) -> EditorM ()
repeatGotoCharE mutateDir = do
    prevCommand <- fmap vsLastGotoCharCommand getDynamic
    count <- getCountE
    withBuffer0 $ case prevCommand of
        Just (GotoCharCommand c dir style) -> do
            let newDir = mutateDir dir
            let move = gotoCharacterB c newDir style True
            p0 <- pointB
            replicateM_ (count - 1) $ do
                move
                when (style == Exclusive) $ moveB Character newDir
            p1 <- pointB
            move
            p2 <- pointB
            when (p1 == p2) $ moveTo p0
        Nothing -> return ()

enableVisualE :: RegionStyle -> EditorM ()
enableVisualE style = withBuffer0 $ do
    putA regionStyleA style
    putA rectangleSelectionA $ Block == style
    setVisibleSelection True
    pointB >>= setSelectionMarkPointB

cutCharE :: Direction -> Int -> EditorM ()
cutCharE dir count = do
    r <- withBuffer0 $ do
        p0 <- pointB
        (if dir == Forward then moveXorEol else moveXorSol) count
        p1 <- pointB
        let region = mkRegion p0 p1
        rope <- readRegionB' region
        deleteRegionB $ mkRegion p0 p1
        leftOnEol
        return rope
    setDefaultRegisterE Inclusive r

tabTraversalBinding :: VimBinding
tabTraversalBinding = VimBindingE prereq action
    where prereq "g" (VimState { vsMode = Normal }) = PartialMatch
          prereq ('g':c:[]) (VimState { vsMode = Normal }) | c `elem` "tT" = WholeMatch ()
          prereq _ _ = NoMatch
          action ('g':c:[]) = do
              count <- getCountE
              replicateM_ count $ if c == 'T' then previousTabE else nextTabE
              resetCountE
              return Drop

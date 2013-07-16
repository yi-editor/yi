module Yi.Keymap.Vim2.NormalMap
    ( defNormalMap
    ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Data.Char
import Data.List (group)
import Data.Maybe (fromMaybe)
import qualified Data.Rope as R

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Eval
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.Operator
import Yi.Keymap.Vim2.Search
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.Utils
import Yi.MiniBuffer
import Yi.Misc
import Yi.Regex (seInput, makeSearchOptsM)
import Yi.Search (getRegexE, isearchInitE, setRegexE, makeSimpleSearch)

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = mkBindingE Normal Continue (char c, return (), mutate)
    where mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
          mutate vs@(VimState {vsCount = Just count}) = vs { vsCount = Just $ count * 10 + d }
          d = ord c - ord '0'

defNormalMap :: [VimBinding]
defNormalMap = pureBindings

pureBindings :: [VimBinding]
pureBindings =
    [zeroBinding, repeatBinding, motionBinding, searchBinding] ++
    [chooseRegisterBinding, setMarkBinding] ++
    fmap mkDigitBinding ['1' .. '9'] ++
    operatorBindings ++
    finishingBingings ++
    continuingBindings ++
    nonrepeatableBindings ++
    jumpBindings ++
    [tabTraversalBinding]

motionBinding :: VimBinding
motionBinding = mkMotionBinding Drop $
    \m -> case m of
        Normal -> True
        _ -> False

chooseRegisterBinding :: VimBinding
chooseRegisterBinding = mkChooseRegisterBinding ((== Normal) . vsMode)

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

jumpBindings :: [VimBinding]
jumpBindings = fmap (mkBindingE Normal Drop)
    [ (ctrlCh 'o', jumpBackE, id)
    , (spec KTab, jumpForwardE, id)
    ]

finishingBingings :: [VimBinding]
finishingBingings = fmap (mkBindingE Normal Finish)
    [ (char 'x', cutCharE Forward =<< getCountE, resetCount)
    , (char 'X', cutCharE Backward =<< getCountE, resetCount)

    , (char 'D',
        do region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
           discard $ operatorApplyToRegionE opDelete 1 $ StyledRegion Exclusive region
        , id)

    -- Pasting
    , (char 'p', pasteAfter, id)
    , (char 'P', pasteBefore, id)

    -- Miscellaneous.
    , (char '~', do
           count <- getCountE
           withBuffer0 $ do
               transformCharactersInLineN count switchCaseChar
               leftOnEol
        , resetCount)
    , (char 'J', do
        count <- fmap (flip (-) 1 . max 2) getCountE
        withBuffer0 $ do
            (StyledRegion s r) <- case stringToMove "j" of
                WholeMatch m -> regionOfMoveB $ CountedMove (Just count) m
                _ -> error "can't happen"
            discard $ lineMoveRel $ count - 1
            moveToEol
            joinLinesB =<< convertRegionToStyleB r s
       , resetCount)
    ]

pasteBefore :: EditorM ()
pasteBefore = do
    -- TODO: use count
    register <- getRegisterE . vsActiveRegister =<< getDynamic
    case register of
        Nothing -> return ()
        Just (Register LineWise rope) -> withBuffer0 $ when (not $ R.null rope) $
            -- Beware of edge cases ahead
            insertRopeWithStyleB (addNewLineIfNecessary rope) LineWise
        Just (Register style rope) -> withBuffer0 $ pasteInclusiveB rope style

pasteAfter :: EditorM ()
pasteAfter = do
    -- TODO: use count
    register <- getRegisterE . vsActiveRegister =<< getDynamic
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
            whenM (fmap not atEol) rightB
            pasteInclusiveB rope style

operatorBindings :: [VimBinding]
operatorBindings = fmap mkOperatorBinding operators
    where mkOperatorBinding (VimOperator {operatorName = opName}) =
              mkStringBindingE Normal (if opName == "y" then Drop else Continue)
                  (opName, return (), switchMode (NormalOperatorPending opName))

continuingBindings :: [VimBinding]
continuingBindings = fmap (mkBindingE Normal Continue)
    [ (char 'r', return (), switchMode ReplaceSingleChar) -- TODO make it just a binding

    -- Transition to insert mode
    , (char 'i', return (), switchMode $ Insert 'i')
    , (char 'I', withBuffer0 firstNonSpaceB, switchMode $ Insert 'I')
    , (char 'a', withBuffer0 rightB, switchMode $ Insert 'a')
    , (char 'A', withBuffer0 moveToEol, switchMode $ Insert 'A')
    , (char 'o', withBuffer0 $ do
                     moveToEol
                     newlineAndIndentB
        , switchMode $ Insert 'o')
    , (char 'O', withBuffer0 $ do
                     moveToSol
                     newlineB
                     leftB
                     indentAsNextB
        , switchMode $ Insert 'O')

    -- Transition to visual
    , (char 'v', enableVisualE Inclusive, resetCount . switchMode (Visual Inclusive))
    , (char 'V', enableVisualE LineWise, resetCount . switchMode (Visual LineWise))
    , (ctrlCh 'v', enableVisualE Block, resetCount . switchMode (Visual Block))
    ]

nonrepeatableBindings :: [VimBinding]
nonrepeatableBindings = fmap (mkBindingE Normal Drop)
    [ (spec KEsc, return (), resetCount)
    , (ctrlCh 'c', return (), resetCount)

    -- Changing
    , (char 'C',
        do region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
           discard $ operatorApplyToRegionE opDelete 1 $ StyledRegion Exclusive region
        , switchMode $ Insert 'C')
    , (char 's', cutCharE Forward =<< getCountE, switchMode $ Insert 's')
    , (char 'S',
        do region <- withBuffer0 $ regionWithTwoMovesB firstNonSpaceB moveToEol
           discard $ operatorApplyToRegionE opDelete 1 $ StyledRegion Exclusive region
        , switchMode $ Insert 'S')

    -- Replacing
    , (char 'R', return (), switchMode Replace)

    -- Yanking
    , (char 'Y',
        do region <- withBuffer0 $ regionWithTwoMovesB (return ()) moveToEol
           discard $ operatorApplyToRegionE opYank 1 $ StyledRegion Exclusive region
        , id)

    -- Search
    , (char '*', addJumpHereE >> searchWordE True Forward, resetCount)
    , (char '#', addJumpHereE >> searchWordE True Backward, resetCount)
    , (char 'n', addJumpHereE >> (withCount $ continueSearching id), resetCount)
    , (char 'N', addJumpHereE >> (withCount $ continueSearching reverseDir), resetCount)
    , (char ';', repeatGotoCharE id, id)
    , (char ',', repeatGotoCharE reverseDir, id)

    -- Repeat
    , (char '&', return (), id) -- TODO

    -- Transition to ex
    , (char ':', discard $ spawnMinibufferE ":" id, switchMode Ex)

    -- Undo
    , (char 'u', withCountOnBuffer0 undoB >> withBuffer0 leftOnEol, id)
    , (char 'U', withCountOnBuffer0 undoB >> withBuffer0 leftOnEol, id) -- TODO
    , (ctrlCh 'r', withCountOnBuffer0 redoB, id)

    -- scrolling
    ,(ctrlCh 'b', getCountE >>= withBuffer0 . upScreensB, id)
    ,(ctrlCh 'f', getCountE >>= withBuffer0 . downScreensB, id)
    ,(ctrlCh 'u', getCountE >>= withBuffer0 . vimScrollByB (negate . (`div` 2)), id)
    ,(ctrlCh 'd', getCountE >>= withBuffer0 . vimScrollByB (`div` 2), id)
    ,(ctrlCh 'y', getCountE >>= withBuffer0 . vimScrollB . negate, id)
    ,(ctrlCh 'e', getCountE >>= withBuffer0 . vimScrollB, id)

    -- unsorted TODO
    , (char '-', return (), id)
    , (char '+', return (), id)
    , (char 'q', return (), id)
    , (spec KEnter, return (), id)
    ] ++ fmap (mkStringBindingE Normal Drop)
    [ ("g*", searchWordE False Forward, resetCount)
    , ("g#", searchWordE False Backward, resetCount)
    , ("<C-g>", printFileInfoE, resetCount)
    , ("<C-w>c", tryCloseE, resetCount)
    , ("<C-w>o", closeOtherE, resetCount)
    , ("<C-w>s", splitE, resetCount)
    , ("<C-w>w", nextWinE, resetCount)
    , ("<C-w><C-w>", nextWinE, resetCount)
    , ("<C-w>W", prevWinE, resetCount)
    , ("<C-w>p", prevWinE, resetCount)
    ]

setMarkBinding :: VimBinding
setMarkBinding = VimBindingE prereq action
    where prereq _ s | vsMode s /= Normal = NoMatch
          prereq "m" _ = PartialMatch
          prereq ('m':_:[]) _ = WholeMatch ()
          prereq _ _ = NoMatch
          action ('m':c:[]) = do
              withBuffer0 $ setNamedMarkHereB [c]
              return Drop
          action _ = error "Can't happen"

searchWordE :: Bool -> Direction -> EditorM ()
searchWordE wholeWord dir = do
    word <- withBuffer0 readCurrentWordB

    let search re = do
            setRegexE re
            putA searchDirectionA dir
            withCount $ continueSearching (const dir)

    if wholeWord
    then case makeSearchOptsM [] $ "\\<" ++ word ++ "\\>" of
            Right re -> search re
            Left _ -> return ()
    else search $ makeSimpleSearch word


searchBinding :: VimBinding
searchBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = Normal }) = matchFromBool $ evs `elem` group "/?"
          prereq _ _ = NoMatch
          action evs = do
              state <- fmap vsMode getDynamic
              let dir = if evs == "/" then Forward else Backward
              switchModeE $ Search "" state dir
              isearchInitE dir
              return Continue

continueSearching :: (Direction -> Direction) -> EditorM ()
continueSearching fdir = do
    mbRegex <- getRegexE
    case mbRegex of
        Just regex -> do
            dir <- fdir <$> getA searchDirectionA
            printMsg $ (if dir == Forward then '/' else '?') : seInput regex
            discard $ doVimSearch Nothing [] dir
        Nothing -> printMsg "No previous search pattern"

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
    regName <- fmap vsActiveRegister getDynamic
    setRegisterE regName Inclusive r

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
          action _ = error "can't happen"

-- TODO: withCount name implies that parameter has type (Int -> EditorM ())
--       Is there a better name for this function?
withCount :: EditorM () -> EditorM ()
withCount action = flip replicateM_ action =<< getCountE

withCountOnBuffer0 :: BufferM () -> EditorM ()
withCountOnBuffer0 action = withCount $ withBuffer0 action

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.NormalMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.NormalMap (defNormalMap) where

import           Prelude                    hiding (lookup)

import           Control.Applicative        ((<$>))
import           Control.Lens               (assign, use, (.=))
import           Control.Monad              (replicateM_, unless, void, when)
import           Data.Char                  (ord)
import           Data.HashMap.Strict        (lookup, singleton)
import           Data.List                  (group)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Monoid (mempty), (<>))
import qualified Data.Text                  as T (drop, empty, pack, replicate, unpack)
import           System.Directory           (doesFileExist)
import           System.FriendlyPath        (expandTilda)
import           Yi.Buffer.Adjusted         hiding (Insert)
import           Yi.Core                    (closeWindow, quitEditor)
import           Yi.Editor
import           Yi.Event                   (Event (Event), Key (KASCII, KEnter, KEsc, KTab), Modifier (MCtrl))
import           Yi.File                    (fwriteE, openNewFile)
import           Yi.History                 (historyPrefixSet, historyStart)
import           Yi.Keymap                  (YiM)
import           Yi.Keymap.Keys             (char, ctrlCh, spec)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Eval         (scheduleActionStringForEval)
import           Yi.Keymap.Vim.Motion       (CountedMove (CountedMove), regionOfMoveB, stringToMove)
import           Yi.Keymap.Vim.Operator     (VimOperator (..), opChange, opDelete, opYank)
import           Yi.Keymap.Vim.Search       (doVimSearch)
import           Yi.Keymap.Vim.StateUtils
import           Yi.Keymap.Vim.StyledRegion (StyledRegion (StyledRegion), transformCharactersInLineN)
import           Yi.Keymap.Vim.Tag          (gotoTag, popTag)
import           Yi.Keymap.Vim.Utils
import           Yi.MiniBuffer              (spawnMinibufferE)
import           Yi.Misc                    (printFileInfoE)
import           Yi.Monad                   (maybeM, whenM)
import           Yi.Regex                   (makeSearchOptsM, seInput)
import qualified Yi.Rope                    as R (fromText, null, toString, toText)
import           Yi.Search                  (getRegexE, isearchInitE, makeSimpleSearch, setRegexE)
import           Yi.String                  (showT)
import           Yi.Tag                     (Tag (..))
import           Yi.Utils                   (io)

data EOLStickiness = Sticky | NonSticky deriving Eq

mkDigitBinding :: Char -> VimBinding
mkDigitBinding c = mkBindingE Normal Continue (char c, return (), mutate)
  where
    mutate vs@(VimState {vsCount = Nothing}) = vs { vsCount = Just d }
    mutate vs@(VimState {vsCount = Just count}) =
      vs { vsCount = Just $ count * 10 + d }
    d = ord c - ord '0'

defNormalMap :: [VimOperator] -> [VimBinding]
defNormalMap operators =
    [recordMacroBinding, finishRecordingMacroBinding, playMacroBinding] <>
    [zeroBinding, repeatBinding, motionBinding, searchBinding] <>
    [chooseRegisterBinding, setMarkBinding] <>
    fmap mkDigitBinding ['1' .. '9'] <>
    operatorBindings operators <>
    finishingBingings <>
    continuingBindings <>
    nonrepeatableBindings <>
    jumpBindings <>
    fileEditBindings <>
    [tabTraversalBinding] <>
    [tagJumpBinding, tagPopBinding]

tagJumpBinding :: VimBinding
tagJumpBinding = mkBindingY Normal (Event (KASCII ']') [MCtrl], f, id)
   where f = withCurrentBuffer readCurrentWordB >>= g . Tag . R.toText
         g tag = gotoTag tag 0 Nothing

tagPopBinding :: VimBinding
tagPopBinding = mkBindingY Normal (Event (KASCII 't') [MCtrl], f, id)
   where f = popTag

motionBinding :: VimBinding
motionBinding = mkMotionBinding Drop $
    \m -> case m of
        Normal -> True
        _ -> False

chooseRegisterBinding :: VimBinding
chooseRegisterBinding = mkChooseRegisterBinding ((== Normal) . vsMode)

zeroBinding :: VimBinding
zeroBinding = VimBindingE f
    where f "0" (VimState {vsMode = Normal}) = WholeMatch $ do
              currentState <- getEditorDyn
              case vsCount currentState of
                  Just c -> do
                      setCountE (10 * c)
                      return Continue
                  Nothing -> do
                      withCurrentBuffer moveToSol
                      resetCountE
                      withCurrentBuffer $ stickyEolA .= False
                      return Drop
          f _ _ = NoMatch

repeatBinding :: VimBinding
repeatBinding = VimBindingE (f . T.unpack . _unEv)
  where
    f "." (VimState {vsMode = Normal}) = WholeMatch $ do
      currentState <- getEditorDyn
      case vsRepeatableAction currentState of
          Nothing -> return ()
          Just (RepeatableAction prevCount (Ev actionString)) -> do
              let count = showT $ fromMaybe prevCount (vsCount currentState)
              scheduleActionStringForEval . Ev $ count <> actionString
              resetCountE
      return Drop
    f _ _ = NoMatch

jumpBindings :: [VimBinding]
jumpBindings = fmap (mkBindingE Normal Drop)
    [ (ctrlCh 'o', jumpBackE, id)
    , (spec KTab, jumpForwardE, id)
    , (ctrlCh '^', controlCarrot, resetCount)
    , (ctrlCh '6', controlCarrot, resetCount)
    ]
  where
    controlCarrot = alternateBufferE . (+ (-1)) =<< getCountE

finishingBingings :: [VimBinding]
finishingBingings = fmap (mkStringBindingE Normal Finish)
    [ ("x", cutCharE Forward NonSticky =<< getCountE, resetCount)
    , ("<Del>", cutCharE Forward NonSticky =<< getCountE, resetCount)
    , ("X", cutCharE Backward NonSticky =<< getCountE, resetCount)

    , ("D",
        do region <- withCurrentBuffer $ regionWithTwoMovesB (return ()) moveToEol
           void $ operatorApplyToRegionE opDelete 1 $ StyledRegion Exclusive region
        , id)

    -- Pasting
    , ("p", pasteAfter, id)
    , ("P", pasteBefore, id)

    -- Miscellaneous.
    , ("~", do
           count <- getCountE
           withCurrentBuffer $ do
               transformCharactersInLineN count switchCaseChar
               leftOnEol
        , resetCount)
    , ("J", do
        count <- fmap (flip (-) 1 . max 2) getCountE
        withCurrentBuffer $ do
            (StyledRegion s r) <- case stringToMove "j" of
                WholeMatch m -> regionOfMoveB $ CountedMove (Just count) m
                _ -> error "can't happen"
            void $ lineMoveRel $ count - 1
            moveToEol
            joinLinesB =<< convertRegionToStyleB r s
       , resetCount)
    ]

pasteBefore :: EditorM ()
pasteBefore = do
    -- TODO: use count
    register <- getRegisterE . vsActiveRegister =<< getEditorDyn
    case register of
        Nothing -> return ()
        Just (Register LineWise rope) -> withCurrentBuffer $ unless (R.null rope) $
            -- Beware of edge cases ahead
            insertRopeWithStyleB (addNewLineIfNecessary rope) LineWise
        Just (Register style rope) -> withCurrentBuffer $ pasteInclusiveB rope style

pasteAfter :: EditorM ()
pasteAfter = do
    -- TODO: use count
    register <- getRegisterE . vsActiveRegister =<< getEditorDyn
    case register of
        Nothing -> return ()
        Just (Register LineWise rope) -> withCurrentBuffer $ do
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
        Just (Register style rope) -> withCurrentBuffer $ do
            whenM (fmap not atEol) rightB
            pasteInclusiveB rope style

operatorBindings :: [VimOperator] -> [VimBinding]
operatorBindings = fmap mkOperatorBinding
  where
    mkT (Op o) = (Ev o, return (), switchMode . NormalOperatorPending $ Op o)
    mkOperatorBinding (VimOperator {operatorName = opName}) =
      mkStringBindingE Normal Continue $ mkT opName

continuingBindings :: [VimBinding]
continuingBindings = fmap (mkStringBindingE Normal Continue)
    [ ("r", return (), switchMode ReplaceSingleChar) -- TODO make it just a binding

    -- Transition to insert mode
    , ("i", return (), switchMode $ Insert 'i')
    , ("<Ins>", return (), switchMode $ Insert 'i')
    , ("I", withCurrentBuffer firstNonSpaceB, switchMode $ Insert 'I')
    , ("a", withCurrentBuffer $ moveXorEol 1, switchMode $ Insert 'a')
    , ("A", withCurrentBuffer moveToEol, switchMode $ Insert 'A')
    , ("o", withCurrentBuffer $ do
          moveToEol
          newlineB
          indentAsTheMostIndentedNeighborLineB
        , switchMode $ Insert 'o')
    , ("O", withCurrentBuffer $ do
                     moveToSol
                     newlineB
                     leftB
                     indentAsNextB
        , switchMode $ Insert 'O')

    -- Transition to visual
    , ("v", enableVisualE Inclusive, resetCount . switchMode (Visual Inclusive))
    , ("V", enableVisualE LineWise, resetCount . switchMode (Visual LineWise))
    , ("<C-v>", enableVisualE Block, resetCount . switchMode (Visual Block))
    ]

nonrepeatableBindings :: [VimBinding]
nonrepeatableBindings = fmap (mkBindingE Normal Drop)
    [ (spec KEsc, return (), resetCount)
    , (ctrlCh 'c', return (), resetCount)

    -- Changing
    , (char 'C',
        do region <- withCurrentBuffer $ regionWithTwoMovesB (return ()) moveToEol
           void $ operatorApplyToRegionE opChange 1 $ StyledRegion Exclusive region
        , switchMode $ Insert 'C')
    , (char 's', cutCharE Forward Sticky =<< getCountE, switchMode $ Insert 's')
    , (char 'S',
        do region <- withCurrentBuffer $ regionWithTwoMovesB firstNonSpaceB moveToEol
           void $ operatorApplyToRegionE opDelete 1 $ StyledRegion Exclusive region
        , switchMode $ Insert 'S')

    -- Replacing
    , (char 'R', return (), switchMode Replace)

    -- Yanking
    , ( char 'Y'
      , do region <- withCurrentBuffer $ regionWithTwoMovesB (return ()) moveToEol
           void $ operatorApplyToRegionE opYank 1 $ StyledRegion Exclusive region
      , id
      )

    -- Search
    , (char '*', addVimJumpHereE >> searchWordE True Forward, resetCount)
    , (char '#', addVimJumpHereE >> searchWordE True Backward, resetCount)
    , (char 'n', addVimJumpHereE >> withCount (continueSearching id), resetCount)
    , (char 'N', addVimJumpHereE >> withCount (continueSearching reverseDir), resetCount)
    , (char ';', repeatGotoCharE id, id)
    , (char ',', repeatGotoCharE reverseDir, id)

    -- Repeat
    , (char '&', return (), id) -- TODO

    -- Transition to ex
    , (char ':', do
        void (spawnMinibufferE ":" id)
        historyStart
        historyPrefixSet ""
      , switchMode Ex)

    -- Undo
    , (char 'u', withCountOnBuffer undoB >> withCurrentBuffer leftOnEol, id)
    , (char 'U', withCountOnBuffer undoB >> withCurrentBuffer leftOnEol, id) -- TODO
    , (ctrlCh 'r', withCountOnBuffer redoB >> withCurrentBuffer leftOnEol, id)

    -- scrolling
    ,(ctrlCh 'b', getCountE >>= withCurrentBuffer . upScreensB, id)
    ,(ctrlCh 'f', getCountE >>= withCurrentBuffer . downScreensB, id)
    ,(ctrlCh 'u', getCountE >>= withCurrentBuffer . vimScrollByB (negate . (`div` 2)), id)
    ,(ctrlCh 'd', getCountE >>= withCurrentBuffer . vimScrollByB (`div` 2), id)
    ,(ctrlCh 'y', getCountE >>= withCurrentBuffer . vimScrollB . negate, id)
    ,(ctrlCh 'e', getCountE >>= withCurrentBuffer . vimScrollB, id)

    -- unsorted TODO
    , (char '-', return (), id)
    , (char '+', return (), id)
    , (spec KEnter, return (), id)
    ] <> fmap (mkStringBindingE Normal Drop)
    [ ("g*", searchWordE False Forward, resetCount)
    , ("g#", searchWordE False Backward, resetCount)
    , ("gd", withCurrentBuffer $ withModeB modeGotoDeclaration, resetCount)
    , ("gD", withCurrentBuffer $ withModeB modeGotoDeclaration, resetCount)
    , ("<C-g>", printFileInfoE, resetCount)
    , ("<C-w>c", tryCloseE, resetCount)
    , ("<C-w>o", closeOtherE, resetCount)
    , ("<C-w>s", splitE, resetCount)
    , ("<C-w>w", nextWinE, resetCount)
    , ("<C-w><Down>", nextWinE, resetCount) -- TODO: please implement downWinE
    , ("<C-w><Right>", nextWinE, resetCount) -- TODO: please implement rightWinE
    , ("<C-w><C-w>", nextWinE, resetCount)
    , ("<C-w>W", prevWinE, resetCount)
    , ("<C-w>p", prevWinE, resetCount)
    , ("<C-w><Up>", prevWinE, resetCount) -- TODO: please implement upWinE
    , ("<C-w><Left>", prevWinE, resetCount) -- TODO: please implement leftWinE
    , ("<C-w>l", layoutManagersNextE, resetCount)
    , ("<C-w>L", layoutManagersPreviousE, resetCount)
    --, ("<C-w> ", layoutManagersNextE, resetCount)
    , ("<C-w>v", layoutManagerNextVariantE, resetCount)
    , ("<C-w>V", layoutManagerPreviousVariantE, resetCount)
    , ("<C-a>", getCountE >>= withCurrentBuffer . incrementNextNumberByB, resetCount)
    , ("<C-x>", getCountE >>= withCurrentBuffer . incrementNextNumberByB . negate, resetCount)

    -- z commands
    -- TODO Add prefix count
    , ("zt", withCurrentBuffer scrollCursorToTopB, resetCount)
    , ("zb", withCurrentBuffer scrollCursorToBottomB, resetCount)
    , ("zz", withCurrentBuffer scrollToCursorB, resetCount)
    {- -- TODO Horizantal scrolling
    , ("ze", withCurrentBuffer .., resetCount)
    , ("zs", withCurrentBuffer .., resetCount)
    , ("zH", withCurrentBuffer .., resetCount)
    , ("zL", withCurrentBuffer .., resetCount)
    , ("zh", withCurrentBuffer .., resetCount)
    , ("zl", withCurrentBuffer .., resetCount)
    -}
    , ("z.", withCurrentBuffer $ scrollToCursorB >> moveToSol, resetCount)
    , ("z+", withCurrentBuffer scrollToLineBelowWindowB, resetCount)
    , ("z-", withCurrentBuffer $ scrollCursorToBottomB >> moveToSol, resetCount)
    , ("z^", withCurrentBuffer scrollToLineAboveWindowB, resetCount)
    {- -- TODO Code folding
    , ("zf", .., resetCount)
    , ("zc", .., resetCount)
    , ("zo", .., resetCount)
    , ("za", .., resetCount)
    , ("zC", .., resetCount)
    , ("zO", .., resetCount)
    , ("zA", .., resetCount)
    , ("zr", .., resetCount)
    , ("zR", .., resetCount)
    , ("zm", .., resetCount)
    , ("zM", .., resetCount)
    -}

    -- Z commands
    ] <> fmap (mkStringBindingY Normal)
    [ ("ZQ", quitEditor, id)
    -- TODO ZZ should replicate :x not :wq
    , ("ZZ", fwriteE >> closeWindow, id)
    ]

fileEditBindings :: [VimBinding]
fileEditBindings =  fmap (mkStringBindingY Normal)
    [ ("gf", openFileUnderCursor Nothing, resetCount)
    , ("<C-w>gf", openFileUnderCursor $ Just newTabE, resetCount)
    , ("<C-w>f", openFileUnderCursor $ Just (splitE >> prevWinE), resetCount)
    ]

setMarkBinding :: VimBinding
setMarkBinding = VimBindingE (f . T.unpack . _unEv)
    where f _ s | vsMode s /= Normal = NoMatch
          f "m" _ = PartialMatch
          f ('m':c:[]) _ = WholeMatch $ do
              withCurrentBuffer $ setNamedMarkHereB [c]
              return Drop
          f _ _ = NoMatch

searchWordE :: Bool -> Direction -> EditorM ()
searchWordE wholeWord dir = do
  word <- withCurrentBuffer readCurrentWordB

  let search re = do
        setRegexE re
        assign searchDirectionA dir
        withCount $ continueSearching (const dir)

  if wholeWord
  then case makeSearchOptsM [] $ "\\<" <> R.toString word <> "\\>" of
          Right re -> search re
          Left _ -> return ()
  else search $ makeSimpleSearch word

searchBinding :: VimBinding
searchBinding = VimBindingE (f . T.unpack . _unEv)
    where f evs (VimState { vsMode = Normal }) | evs `elem` group ['/', '?']
            = WholeMatch $ do
                  state <- fmap vsMode getEditorDyn
                  let dir = if evs == "/" then Forward else Backward
                  switchModeE $ Search state dir
                  isearchInitE dir
                  historyStart
                  historyPrefixSet T.empty
                  return Continue
          f _ _ = NoMatch

continueSearching :: (Direction -> Direction) -> EditorM ()
continueSearching fdir =
  getRegexE >>= \case
    Just regex -> do
      dir <- fdir <$> use searchDirectionA
      printMsg . T.pack $ (if dir == Forward then '/' else '?') : seInput regex
      void $ doVimSearch Nothing [] dir
    Nothing -> printMsg "No previous search pattern"

repeatGotoCharE :: (Direction -> Direction) -> EditorM ()
repeatGotoCharE mutateDir = do
    prevCommand <- fmap vsLastGotoCharCommand getEditorDyn
    count <- getCountE
    withCurrentBuffer $ case prevCommand of
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
enableVisualE style = withCurrentBuffer $ do
    putRegionStyle style
    rectangleSelectionA .= (Block == style)
    setVisibleSelection True
    pointB >>= setSelectionMarkPointB

cutCharE :: Direction -> EOLStickiness -> Int -> EditorM ()
cutCharE dir stickiness count = do
    r <- withCurrentBuffer $ do
        p0 <- pointB
        (if dir == Forward then moveXorEol else moveXorSol) count
        p1 <- pointB
        let region = mkRegion p0 p1
        rope <- readRegionB region
        deleteRegionB $ mkRegion p0 p1
        when (stickiness == NonSticky) leftOnEol
        return rope
    regName <- fmap vsActiveRegister getEditorDyn
    setRegisterE regName Inclusive r

tabTraversalBinding :: VimBinding
tabTraversalBinding = VimBindingE (f . T.unpack . _unEv)
    where f "g" (VimState { vsMode = Normal }) = PartialMatch
          f ('g':c:[]) (VimState { vsMode = Normal }) | c `elem` ['t', 'T'] = WholeMatch $ do
              count <- getCountE
              replicateM_ count $ if c == 'T' then previousTabE else nextTabE
              resetCountE
              return Drop
          f _ _ = NoMatch

openFileUnderCursor :: Maybe (EditorM ()) -> YiM ()
openFileUnderCursor editorAction = do
  fileName <- fmap R.toString . withCurrentBuffer $ readUnitB unitViWORD
  fileExists <- io $ doesFileExist =<< expandTilda fileName
  if fileExists then do
      maybeM withEditor editorAction
      openNewFile $ fileName
  else
      withEditor . fail $ "Can't find file \"" <> fileName <> "\""

recordMacroBinding :: VimBinding
recordMacroBinding = VimBindingE (f . T.unpack . _unEv)
    where f "q" (VimState { vsMode = Normal
                          , vsCurrentMacroRecording = Nothing })
                = PartialMatch
          f ['q', c] (VimState { vsMode = Normal })
              = WholeMatch $ do
                    modifyStateE $ \s ->
                        s { vsCurrentMacroRecording = Just (c, mempty) }
                    return Finish
          f _ _ = NoMatch

finishRecordingMacroBinding :: VimBinding
finishRecordingMacroBinding = VimBindingE (f . T.unpack . _unEv)
    where f "q" (VimState { vsMode = Normal
                          , vsCurrentMacroRecording = Just (macroName, Ev macroBody) })
                = WholeMatch $ do
                      let reg = Register Exclusive (R.fromText (T.drop 2 macroBody))
                      modifyStateE $ \s ->
                          s { vsCurrentMacroRecording = Nothing
                              , vsRegisterMap = singleton macroName reg
                                              <> vsRegisterMap s
                              }
                      return Finish
          f _ _ = NoMatch

playMacroBinding :: VimBinding
playMacroBinding = VimBindingE (f . T.unpack . _unEv)
    where f "@" (VimState { vsMode = Normal }) = PartialMatch
          f ['@', c] (VimState { vsMode = Normal
                               , vsRegisterMap = registers
                               , vsCount = mbCount }) = WholeMatch $ do
              resetCountE
              case lookup c registers of
                  Just (Register _ evs) -> do
                      let count = fromMaybe 1 mbCount
                          mkAct = Ev . T.replicate count . R.toText
                      scheduleActionStringForEval . mkAct $ evs
                      return Finish
                  Nothing -> return Drop
          f _ _ = NoMatch

-- TODO: withCount name implies that parameter has type (Int -> EditorM ())
--       Is there a better name for this function?
withCount :: EditorM () -> EditorM ()
withCount action = flip replicateM_ action =<< getCountE

withCountOnBuffer :: BufferM () -> EditorM ()
withCountOnBuffer action = withCount $ withCurrentBuffer action

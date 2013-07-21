module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)
import Data.Char (isDigit)
import Data.List (break, drop, dropWhile)
import Data.Maybe (maybe)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Digraph
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Motion
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.StateUtils
import Yi.TextCompletion (completeWordB)

defInsertMap :: [(String, Char)] -> [VimBinding]
defInsertMap digraphs = specials digraphs ++ [printable]

specials :: [(String, Char)] -> [VimBinding]
specials digraphs =
    [exitBinding digraphs, pasteRegisterBinding, digraphBinding digraphs
    , oneshotNormalBinding, completionBinding, cursorBinding]

-- exitBinding :: VimBinding
exitBinding digraphs = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Insert _) }) =
              matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              count <- getCountE
              (Insert starter) <- fmap vsMode getDynamic
              when (count > 1) $ do
                  inputEvents <- fmap (parseEvents . vsOngoingInsertEvents) getDynamic
                  replicateM_ (count - 1) $ do
                      when (starter `elem` "Oo") $ withBuffer0 $ insertB '\n'
                      replay digraphs inputEvents
              modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
              withBuffer0 $ moveXorSol 1
              modifyStateE $ \s -> s { vsSecondaryCursors = [] }
              resetCountE
              switchModeE Normal
              withBuffer0 $ do
                  whenM isCurrentLineAllWhiteSpaceB $ moveToSol >> deleteToEol
              return Finish

-- replay :: [Event] -> EditorM ()
replay _ [] = return ()
replay digraphs (e1:es1) = do
    state <- getDynamic
    let recurse = replay digraphs
        evs1 = eventToString e1
        bindingMatch1 = selectBinding evs1 state (defInsertMap digraphs)
    case bindingMatch1 of
        WholeMatch (VimBindingE _ action) -> discard (action evs1) >> recurse es1
        PartialMatch -> do
            case es1 of
                [] -> return ()
                (e2:es2) -> do
                    let evs2 = evs1 ++ eventToString e2
                        bindingMatch2 = selectBinding evs2 state (defInsertMap digraphs)
                    case bindingMatch2 of
                        WholeMatch (VimBindingE _ action) -> discard (action evs2) >> recurse es2
                        _ -> recurse es2
        _ -> recurse es1

oneshotNormalBinding :: VimBinding
oneshotNormalBinding = VimBindingE prereq action
    where prereq "<C-o>" (VimState { vsMode = Insert _ }) = PartialMatch
          prereq ('<':'C':'-':'o':'>':evs) (VimState { vsMode = Insert _ }) =
              fmap (const ()) (stringToMove (dropWhile isDigit evs))
          prereq _ _ = NoMatch
          action ('<':'C':'-':'o':'>':evs) = do
              let (countString, motionCmd) = break (not . isDigit) evs
                  WholeMatch (Move _style _isJump move) = stringToMove motionCmd
              withBuffer0 $ move (if null countString then Nothing else Just (read countString))
              return Continue
          action _ = error "can't happen"

pasteRegisterBinding :: VimBinding
pasteRegisterBinding = VimBindingE prereq action
    where prereq "<C-r>" (VimState { vsMode = Insert _ }) = PartialMatch
          prereq ('<':'C':'-':'r':'>':_c1:[]) (VimState { vsMode = Insert _ }) = WholeMatch ()
          prereq _ _ = NoMatch
          action evs = do
              let regName = last evs
              mr <- getRegisterE regName
              case mr of
                Nothing -> return ()
                Just (Register _style rope) -> withBuffer0 $ do
                    insertRopeWithStyleB rope Inclusive
              return Continue

digraphBinding :: [(String, Char)] -> VimBinding
digraphBinding digraphs = VimBindingE prereq action
    where prereq ('<':'C':'-':'k':'>':_c1:_c2:[]) (VimState { vsMode = Insert _ }) = WholeMatch ()
          prereq ('<':'C':'-':'k':'>':_c1:[]) (VimState { vsMode = Insert _ }) = PartialMatch
          prereq "<C-k>" (VimState { vsMode = Insert _ }) = PartialMatch
          prereq _ _ = NoMatch
          action ('<':'C':'-':'k':'>':c1:c2:[]) = do
              maybe (return ()) (withBuffer0 . insertB) $ charFromDigraph digraphs c1 c2
              return Continue
          action _ = error "can't happen"

-- TODO: split this binding into printable and specials
printable :: VimBinding
printable = VimBindingE prereq printableAction
    where prereq evs state@(VimState { vsMode = Insert _ } ) =
              matchFromBool $
                  all (\b -> case vbPrerequisite b evs state of
                              NoMatch -> True
                              _ -> False) (specials undefined)
          prereq _ _ = NoMatch

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    saveInsertEventStringE evs
    currentCursor <- withBuffer0 pointB
    secondaryCursors <- fmap vsSecondaryCursors getDynamic
    let allCursors = currentCursor:secondaryCursors
    marks <- withBuffer0 $ forM allCursors $ \cursor -> do
        moveTo cursor
        -- getMarkB $ Just $ "v_I" ++ show cursor
        getMarkB Nothing
    let bufAction = case evs of
                        (c:[]) -> insertB c
                        "<CR>" -> do
                            isOldLineEmpty <- isCurrentLineEmptyB
                            shouldTrimOldLine <- isCurrentLineAllWhiteSpaceB
                            if isOldLineEmpty
                            then do
                                newlineB
                            else if shouldTrimOldLine
                            then savingPointB $ do
                                moveToSol
                                newlineB
                            else do
                                newlineB
                                indentAsPreviousB
                            firstNonSpaceB
                        -- For testing purposes assume noexpandtab, tw=4
                        "<Tab>" -> insertN $ replicate 4 ' '
                        "<C-t>" -> shiftIndentOfRegion 1 =<< regionOfB Line
                        "<C-d>" -> shiftIndentOfRegion (-1) =<< regionOfB Line
                        "<C-e>" -> insertCharWithBelowB
                        "<C-y>" -> insertCharWithAboveB
                        "<BS>"  -> deleteB Character Backward
                        "<C-h>" -> deleteB Character Backward
                        "<Del>" -> deleteB Character Forward
                        "<C-j>" -> insertB '\n'
                        "<C-w>" -> deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Backward
                        "<lt>" -> insertB '<'
                        evs' -> error $ "Unhandled event " ++ evs' ++ " in insert mode"
    updatedCursors <- withBuffer0 $ do
        updatedCursors <- forM marks $ \mark -> do
            moveTo =<< getMarkPointB mark
            bufAction
            pointB
        mapM_ deleteMarkB marks
        moveTo $ head updatedCursors
        return updatedCursors
    modifyStateE $ \s -> s { vsSecondaryCursors = drop 1 updatedCursors }
    return Continue

completionBinding :: VimBinding
completionBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Insert _) }) =
              matchFromBool $ evs `elem` ["<C-n>", "<C-p>"]
          prereq _ _ = NoMatch
          action evs = do
              let _direction = if evs == "<C-n>" then Forward else Backward
              completeWordB
              return Continue

cursorBinding :: VimBinding
cursorBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Insert _) }) =
              matchFromBool $ evs `elem` ["<Up>", "<Left>", "<Down>", "<Right>"]
          prereq _ _ = NoMatch
          action evs = do
              let WholeMatch (Move _style _isJump move) = stringToMove evs
              withBuffer0 $ move Nothing
              return Continue

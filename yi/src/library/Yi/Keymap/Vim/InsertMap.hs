module Yi.Keymap.Vim.InsertMap
  ( defInsertMap
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List.NonEmpty hiding (drop, span, dropWhile)
import Data.Char (isDigit)
import Prelude hiding (head)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Digraph
import Yi.Keymap.Vim.EventUtils
import Yi.Keymap.Vim.Motion
import Yi.Keymap.Vim.Utils
import Yi.Keymap.Vim.StateUtils
import Yi.TextCompletion (completeWordB, CompletionScope(..))
import Yi.Monad

defInsertMap :: [(String, Char)] -> [VimBinding]
defInsertMap digraphs =
    [rawPrintable] ++ specials digraphs ++ [printable]

specials :: [(String, Char)] -> [VimBinding]
specials digraphs =
    [exitBinding digraphs, pasteRegisterBinding, digraphBinding digraphs
    , oneshotNormalBinding, completionBinding, cursorBinding]

exitBinding :: [(String, Char)] -> VimBinding
exitBinding digraphs = VimBindingE f
    where f evs (VimState { vsMode = (Insert _) })
            | evs `elem` ["<Esc>", "<C-c>"]
            = WholeMatch $ do
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
                  withBuffer0 $ whenM isCurrentLineAllWhiteSpaceB $ moveToSol >> deleteToEol
                  return Finish
          f _ _ = NoMatch

rawPrintable :: VimBinding
rawPrintable = VimBindingE f
    where f evs s@(VimState { vsMode = (Insert _)})
                | vsPaste s && evs `notElem` ["<Esc>", "<C-c>"]
                    = WholeMatch . withBuffer0 $ do
                          case evs of
                              "<lt>" -> insertB '<'
                              "<CR>" -> newlineB
                              "<Tab>" -> insertB '\t'
                              "<BS>"  -> bdeleteB
                              "<C-h>" -> bdeleteB
                              "<Del>" -> deleteB Character Forward
                              c -> insertN c
                          return Continue
          f _ _ = NoMatch

replay :: [(String, Char)] -> [Event] -> EditorM ()
replay _ [] = return ()
replay digraphs (e1:es1) = do
    state <- getDynamic
    let recurse = replay digraphs
        evs1 = eventToString e1
        bindingMatch1 = selectPureBinding evs1 state (defInsertMap digraphs)
    case bindingMatch1 of
        WholeMatch action -> void action >> recurse es1
        PartialMatch -> case es1 of
            [] -> return ()
            (e2:es2) -> do
                let evs2 = evs1 ++ eventToString e2
                    bindingMatch2 = selectPureBinding evs2 state (defInsertMap digraphs)
                case bindingMatch2 of
                    WholeMatch action -> void action >> recurse es2
                    _ -> recurse es2
        _ -> recurse es1

oneshotNormalBinding :: VimBinding
oneshotNormalBinding = VimBindingE f
    where f "<C-o>" (VimState { vsMode = Insert _ }) = PartialMatch
          f ('<':'C':'-':'o':'>':evs) (VimState { vsMode = Insert _ }) =
              action evs <$ stringToMove (dropWhile isDigit evs)
          f _ _ = NoMatch
          action evs = do
              let (countString, motionCmd) = span isDigit evs
                  WholeMatch (Move _style _isJump move) = stringToMove motionCmd
              withBuffer0 $ move (if null countString then Nothing else Just (read countString))
              return Continue

pasteRegisterBinding :: VimBinding
pasteRegisterBinding = VimBindingE f
    where f "<C-r>" (VimState { vsMode = Insert _ }) = PartialMatch
          f ('<':'C':'-':'r':'>':regName:[]) (VimState { vsMode = Insert _ })
              = WholeMatch $ do
                  mr <- getRegisterE regName
                  case mr of
                    Nothing -> return ()
                    Just (Register _style rope) -> withBuffer0 $ insertRopeWithStyleB rope Inclusive
                  return Continue
          f _ _ = NoMatch

digraphBinding :: [(String, Char)] -> VimBinding
digraphBinding digraphs = VimBindingE f
    where f ('<':'C':'-':'k':'>':c1:c2:[]) (VimState { vsMode = Insert _ })
            = WholeMatch $ do
                  maybe (return ()) (withBuffer0 . insertB) $ charFromDigraph digraphs c1 c2
                  return Continue
          f ('<':'C':'-':'k':'>':_c1:[]) (VimState { vsMode = Insert _ }) = PartialMatch
          f "<C-k>" (VimState { vsMode = Insert _ }) = PartialMatch
          f _ _ = NoMatch

printable :: VimBinding
printable = VimBindingE f
    where f evs state@(VimState { vsMode = Insert _ } ) =
              case selectBinding evs state (specials undefined) of
                  NoMatch -> WholeMatch (printableAction evs)
                  _ -> NoMatch
          f _ _ = NoMatch

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    saveInsertEventStringE evs
    currentCursor <- withBuffer0 pointB
    secondaryCursors <- fmap vsSecondaryCursors getDynamic
    let allCursors = currentCursor :| secondaryCursors
    marks <- withBuffer0 $ forM' allCursors $ \cursor -> do
        moveTo cursor
        -- getMarkB $ Just $ "v_I" ++ show cursor
        getMarkB Nothing
    let bufAction = case evs of
                        (c:[]) -> insertB c
                        "<CR>" -> do
                            isOldLineEmpty <- isCurrentLineEmptyB
                            shouldTrimOldLine <- isCurrentLineAllWhiteSpaceB
                            if isOldLineEmpty
                            then newlineB
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
                        "<C-t>" -> shiftIndentOfRegionB 1 =<< regionOfB Line
                        "<C-d>" -> shiftIndentOfRegionB (-1) =<< regionOfB Line
                        "<C-e>" -> insertCharWithBelowB
                        "<C-y>" -> insertCharWithAboveB
                        "<BS>"  -> bdeleteB
                        "<C-h>" -> bdeleteB
                        "<Del>" -> deleteB Character Forward
                        "<C-w>" -> deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Backward
                        "<C-u>" -> bdeleteLineB
                        "<lt>" -> insertB '<'
                        evs' -> error $ "Unhandled event " ++ evs' ++ " in insert mode"
    updatedCursors <- withBuffer0 $ do
        updatedCursors <- forM' marks $ \mark -> do
            moveTo =<< use (markPointA mark)
            bufAction
            pointB
        mapM_ deleteMarkB $ toList marks
        moveTo $ head updatedCursors
        return $ toList updatedCursors
    modifyStateE $ \s -> s { vsSecondaryCursors = drop 1 updatedCursors }
    return Continue
  where
    forM' :: Monad m => NonEmpty a -> (a -> m b) -> m (NonEmpty b)
    forM' (x :| xs) f = liftM2 (:|) (f x) (forM xs f)

completionBinding :: VimBinding
completionBinding = VimBindingE f
    where f evs (VimState { vsMode = (Insert _) })
            | evs `elem` ["<C-n>", "<C-p>"]
            = WholeMatch $ do
                  let _direction = if evs == "<C-n>" then Forward else Backward
                  completeWordB FromAllBuffers
                  return Continue
          f _ _ = NoMatch

cursorBinding :: VimBinding
cursorBinding = VimBindingE f
    where f evs (VimState { vsMode = (Insert _) })
            | evs `elem` ["<Up>", "<Left>", "<Down>", "<Right>"]
            = WholeMatch $ do
                  let WholeMatch (Move _style _isJump move) = stringToMove evs
                  withBuffer0 $ move Nothing
                  return Continue
          f _ _ = NoMatch

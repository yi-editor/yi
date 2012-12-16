module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)
import Data.Maybe (maybe)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.Digraph
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.StateUtils

defInsertMap :: [VimBinding]
defInsertMap = [exitBinding, digraphBinding, printable]

exitBinding :: VimBinding
exitBinding = VimBindingE prereq action
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
                      replay inputEvents
              modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
              withBuffer0 $ moveXorSol 1
              resetCountE
              switchModeE Normal
              return Finish

replay :: [Event] -> EditorM ()
-- TODO: make digraphs work here too
replay = mapM_ (printableAction . eventToString)

digraphBinding :: VimBinding
digraphBinding = VimBindingE prereq action
    where prereq ('<':'C':'-':'k':'>':_c1:_c2:[]) (VimState { vsMode = Insert _ }) = WholeMatch ()
          prereq ('<':'C':'-':'k':'>':_c1:[]) (VimState { vsMode = Insert _ }) = PartialMatch
          prereq "<C-k>" (VimState { vsMode = Insert _ }) = PartialMatch
          prereq _ _ = NoMatch
          action ('<':'C':'-':'k':'>':c1:c2:[]) = do
              maybe (return ()) (withBuffer0 . insertB) $ charFromDigraph c1 c2
              return Continue

-- TODO: split this binding into printable and specials
printable :: VimBinding
printable = VimBindingE prereq action
    where prereq evs state@(VimState { vsMode = Insert _ } ) =
              case vbPrerequisite digraphBinding evs state of
                  NoMatch -> WholeMatch ()
                  _ -> NoMatch
          prereq _ _ = NoMatch
          action = printableAction

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    save evs
    withBuffer0 $ case evs of
        (c:[]) -> insertB c
        "<CR>" -> insertB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        "<Tab>" -> insertN $ replicate 4 ' '
        "<C-t>" -> shiftIndentOfRegion 1 =<< regionOfB Line
        "<C-d>" -> shiftIndentOfRegion (-1) =<< regionOfB Line
        "<C-e>" -> insertCharWithBelowB
        "<C-y>" -> insertCharWithAboveB
        "<BS>"  -> deleteB Character Backward
        "<C-h>" -> deleteB Character Backward
        "<C-j>" -> insertB '\n'
        "<C-o>" -> return () -- TODO
        "<C-w>" -> deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Backward
        "<C-r>" -> return () -- TODO
        "<C-k>" -> return () -- TODO
        evs' -> error $ "Unhandled event " ++ evs' ++ " in insert mode"
    return Continue

save :: EventString -> EditorM ()
save evs =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ evs }

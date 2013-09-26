module Yi.Keymap.Vim2.Ex
    ( exEvalE
    , exEvalY
    , stringToExCommand
    , ExCommand(..)
    , defExCommandParsers
    ) where

import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Eval

import qualified Yi.Keymap.Vim2.Ex.Commands.BufferDelete as BufferDelete
import qualified Yi.Keymap.Vim2.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim2.Ex.Commands.Edit as Edit
import qualified Yi.Keymap.Vim2.Ex.Commands.Global as Global
import qualified Yi.Keymap.Vim2.Ex.Commands.GotoLine as GotoLine
import qualified Yi.Keymap.Vim2.Ex.Commands.Nohl as Nohl
import qualified Yi.Keymap.Vim2.Ex.Commands.Paste as Paste
import qualified Yi.Keymap.Vim2.Ex.Commands.Quit as Quit
import qualified Yi.Keymap.Vim2.Ex.Commands.Reload as Reload
import qualified Yi.Keymap.Vim2.Ex.Commands.Substitute as Substitute
import qualified Yi.Keymap.Vim2.Ex.Commands.Write as Write
import qualified Yi.Keymap.Vim2.Ex.Commands.Yi as Yi

defExCommandParsers :: [String -> Maybe ExCommand]
defExCommandParsers =
    [ BufferDelete.parse
    , Delete.parse
    , Edit.parse
    , Global.parse
    , GotoLine.parse
    , Nohl.parse
    , Paste.parse
    , Quit.parse
    , Reload.parse
    , Substitute.parse
    , Write.parse
    , Yi.parse
    ]

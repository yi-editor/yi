module Yi.Keymap.Vim2.Ex
    ( exEvalE
    , exEvalY
    , stringToExCommand
    , ExCommand(..)
    , ExCommandBox(..)
    , allExCommands
    ) where

import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Eval

import qualified Yi.Keymap.Vim2.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim2.Ex.Commands.Edit as Edit
import qualified Yi.Keymap.Vim2.Ex.Commands.Global as Global
import qualified Yi.Keymap.Vim2.Ex.Commands.GotoLine as GotoLine
import qualified Yi.Keymap.Vim2.Ex.Commands.Nohl as Nohl
import qualified Yi.Keymap.Vim2.Ex.Commands.Quit as Quit
import qualified Yi.Keymap.Vim2.Ex.Commands.Reload as Reload
import qualified Yi.Keymap.Vim2.Ex.Commands.Substitute as Substitute
import qualified Yi.Keymap.Vim2.Ex.Commands.Write as Write
import qualified Yi.Keymap.Vim2.Ex.Commands.Yi as Yi

allExCommands :: [ExCommandBox]
allExCommands = concat
    [ Delete.commands
    , Edit.commands
    , Global.commands
    , GotoLine.commands
    , Nohl.commands
    , Quit.commands
    , Reload.commands
    , Substitute.commands
    , Write.commands
    , Yi.commands
    ]

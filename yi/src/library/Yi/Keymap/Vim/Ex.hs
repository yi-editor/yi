module Yi.Keymap.Vim.Ex
    ( exEvalE
    , exEvalY
    , stringToExCommand
    , ExCommand(..)
    , defExCommandParsers
    ) where

import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.Ex.Eval

import qualified Yi.Keymap.Vim.Ex.Commands.Buffer as Buffer
import qualified Yi.Keymap.Vim.Ex.Commands.Buffers as Buffers
import qualified Yi.Keymap.Vim.Ex.Commands.BufferDelete as BufferDelete
import qualified Yi.Keymap.Vim.Ex.Commands.Cabal as Cabal
import qualified Yi.Keymap.Vim.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim.Ex.Commands.Edit as Edit
import qualified Yi.Keymap.Vim.Ex.Commands.Global as Global
import qualified Yi.Keymap.Vim.Ex.Commands.GotoLine as GotoLine
import qualified Yi.Keymap.Vim.Ex.Commands.Nohl as Nohl
import qualified Yi.Keymap.Vim.Ex.Commands.Paste as Paste
import qualified Yi.Keymap.Vim.Ex.Commands.Quit as Quit
import qualified Yi.Keymap.Vim.Ex.Commands.Reload as Reload
import qualified Yi.Keymap.Vim.Ex.Commands.Substitute as Substitute
import qualified Yi.Keymap.Vim.Ex.Commands.Write as Write
import qualified Yi.Keymap.Vim.Ex.Commands.Yi as Yi

defExCommandParsers :: [String -> Maybe ExCommand]
defExCommandParsers =
    [ Buffer.parse
    , Buffers.parse
    , BufferDelete.parse
    , Cabal.parse
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

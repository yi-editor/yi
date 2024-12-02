{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex
    ( exEvalE
    , exEvalY
    , evStringToExCommand
    , ExCommand(..)
    , defExCommandParsers
    ) where

import           Yi.Keymap.Vim.Common                   (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Buffer       as Buffer (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.BufferCycle  as BufferCycle (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.BufferDelete as BufferDelete (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.BufferNew    as BufferNew (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Buffers      as Buffers (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Cabal        as Cabal (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Delete       as Delete (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Edit         as Edit (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Global       as Global (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.GotoLine     as GotoLine (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Help         as Help (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Make         as Make (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Nohl         as Nohl (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Number       as Number (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Paste        as Paste (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Quit         as Quit (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Read         as Read (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Shell        as Shell (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Sort         as Sort (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Substitute   as Substitute (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Tag          as Tag (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Undo         as Undo (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Write        as Write (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Yi           as Yi (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Copy         as Copy (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Stack        as Stack (parse)
import qualified Yi.Keymap.Vim.Ex.Commands.Registers    as Registers (parse)
import           Yi.Keymap.Vim.Ex.Eval                  (exEvalE, exEvalY)
import           Yi.Keymap.Vim.Ex.Types                 (ExCommand (..), evStringToExCommand)

defExCommandParsers :: [EventString -> Maybe ExCommand]
defExCommandParsers =
    [ Buffer.parse
    , BufferCycle.parse
    , Buffers.parse
    , BufferDelete.parse
    , BufferNew.parse
    , Cabal.parse
    , Delete.parse
    , Edit.parse
    , Global.parse
    , GotoLine.parse
    , Help.parse
    , Make.parse
    , Nohl.parse
    , Number.parse
    , Paste.parse
    , Quit.parse
    , Read.parse
    , Sort.parse
    , Substitute.parse
    , Shell.parse
    , Tag.parse
    , Undo.parse
    , Write.parse
    , Yi.parse
    , Copy.parse
    , Stack.parse
    , Registers.parse
    ]

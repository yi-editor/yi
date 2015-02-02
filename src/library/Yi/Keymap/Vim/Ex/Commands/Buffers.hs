{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.BufferDelete
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- :buffers or :ls ex command to list buffers.
module Yi.Keymap.Vim.Ex.Commands.Buffers (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Lens                     (view)
import           Control.Monad                    (void)
import qualified Data.Map                         as M (elems, mapWithKey)
import qualified Data.Text                        as T (intercalate, pack, unlines)
import qualified Text.ParserCombinators.Parsec    as P (string, try)
import           Yi.Buffer.Basic                  (BufferRef (BufferRef))
import           Yi.Buffer.Misc                   (BufferId (MemBuffer), identA)
import           Yi.Editor
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Monad                         (gets)
import           Yi.Rope                          (fromText)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "buffers") <|> P.try ( P.string "ls") <|> P.try ( P.string "files" )
    return $ Common.pureExCommand {
        cmdShow = "buffers"
      , cmdAction = EditorA $ withEditor printBuffers
      }

printBuffers :: EditorM ()
printBuffers = do
    -- TODO Don't keep recreating new buffers. Use a pre-existing one.
    --      See the cabal buffer used in Command.hs for an example.
    -- TODO Add some simple keymaps to the buffer, like <CR> to open the buffer?
    bufs <- gets buffers
    let bufLines = M.elems $ M.mapWithKey bufLine bufs
    if length bufLines > 1
      then withEditor . void $
             newBufferE (MemBuffer "Buffer list")
                        (fromText $ T.unlines bufLines)
      else printMsgs bufLines
  where
    tab = T.pack "\t"
    -- TODO shorten this name string perhaps.
    -- TODO Add more information: modified status, line number.
    bufLine (BufferRef bufNum) buf =
        T.intercalate tab [ T.pack . show $ bufNum
                          , T.pack . show . view identA $ buf
                          ]

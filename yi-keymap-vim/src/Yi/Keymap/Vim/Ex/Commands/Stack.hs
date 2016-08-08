{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Stack
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Stack (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Data.Attoparsec.Text             as P (choice, Parser)
import           Data.Text                        (Text)
import           Data.Monoid                      ((<>))
import           Yi.Command                       (stackCommandE)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (commandArgs, impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.MiniBuffer                    (CommandArguments (CommandArguments))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    cmd <- "stack" *> (" " *> P.choice commands <|> pure "build")
    args <- Common.commandArgs
    return $ Common.impureExCommand {
        cmdShow = "stack " <> cmd
      , cmdAction = YiA $ stackCommandE cmd $ CommandArguments args
      }

commands :: [P.Parser Text]
commands =
  [ "build"
  , "install"
  , "uninstall"
  , "test"
  , "bench"
  , "haddock"
  , "new"
  , "templates"
  , "init"
  , "solver"
  , "setup"
  , "path"
  , "unpack"
  , "update"
  , "upgrade"
  , "upload"
  , "sdist"
  , "dot"
  , "exec"
  , "ghc"
  , "ghci"
  , "repl"
  , "runghc"
  , "runhaskell"
  , "eval"
  , "clean"
  , "list-dependencies"
  , "query"
  , "ide"
  , "docker"
  , "config"
  , "image"
  , "hpc"
  ]
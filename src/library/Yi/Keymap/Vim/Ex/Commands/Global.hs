{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Global
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Global (parse) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer.Adjusted
import           Yi.Editor
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import qualified Yi.Keymap.Vim.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim.Ex.Commands.Substitute as Substitute
import           Yi.Keymap.Vim.Ex.Types
import qualified Yi.Rope as R
import           Yi.String (showT)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try (P.string "global/") <|> P.string "g/"
    predicate <- T.pack <$> P.many (P.noneOf "/")
    void $ P.char '/'
    cmdString <- Ev . T.pack <$> P.many P.anyChar
    cmd <- case evStringToExCommand allowedCmds cmdString of
            Just c -> return c
            _ -> fail "Unexpected command argument for global command."
    return $! global predicate cmd

global :: T.Text -> ExCommand -> ExCommand
global p c = Common.pureExCommand {
    cmdShow = "g/" <> p `T.snoc` '/' <> showT c
  , cmdAction = EditorA $ do
        mark <- withCurrentBuffer setMarkHereB
        lineCount <- withCurrentBuffer lineCountB
        forM_ (reverse [1..lineCount]) $ \l -> do
            ln <- withCurrentBuffer $ gotoLn l >> R.toText <$> readLnB
            when (p `T.isInfixOf` ln) $
                case cmdAction c of
                    BufferA action -> withCurrentBuffer $ void action
                    EditorA action -> void action
                    _ -> error "Impure command as an argument to global."
        withCurrentBuffer $ do
            use (markPointA mark) >>= moveTo
            deleteMarkB mark
  }

allowedCmds :: [EventString -> Maybe ExCommand]
allowedCmds = [Delete.parse, Substitute.parse]

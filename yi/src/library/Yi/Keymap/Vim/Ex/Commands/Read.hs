{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Read
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Read (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad.Base               (liftBase)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (Text, pack)
import           System.Exit                      (ExitCode (..))
import qualified Text.ParserCombinators.Parsec    as P (anyChar, many1, space, string, try)
import           Yi.Buffer.HighLevel              (insertRopeWithStyleB)
import           Yi.Buffer.Normal                 (RegionStyle (LineWise))
import           Yi.Editor                        (printMsg, withCurrentBuffer)
import           Yi.Keymap                        (Action (YiA), YiM)
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Process                       (runShellCommand)
import           Yi.Rope                          (fromString, YiString)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $
               (P.try (P.string "read") <|> P.string "r") *> P.many1 P.space
            *> ((P.string "!" *> parseCommand) <|> parseReadFile)
    where parseReadFile = do
            filename <- P.many1 P.anyChar
            return $! readCmd ("read file " <> T.pack filename)
                              (liftBase $ fromString <$> readFile filename)
          parseCommand = do
            command <- P.many1 P.anyChar
            return $! readCmd ("read command " <> T.pack command) (runShellCommand' command)
          runShellCommand' :: String -> YiM YiString
          runShellCommand' cmd = do
            (exitCode,cmdOut,cmdErr) <- liftBase $ runShellCommand cmd
            case exitCode of
              ExitSuccess -> return $ fromString cmdOut
              -- FIXME: here we get a string and convert it back to utf8;
              -- this indicates a possible bug (This is copied from 'shellCommandV').
              ExitFailure _ -> printMsg (T.pack cmdErr) >> return ""

readCmd :: T.Text -> YiM YiString -> ExCommand
readCmd cmdShowText getYiString = Common.impureExCommand
  { cmdShow = cmdShowText
  , cmdAction = YiA $ do
      s <- getYiString
      withCurrentBuffer $ insertRopeWithStyleB s LineWise
  }

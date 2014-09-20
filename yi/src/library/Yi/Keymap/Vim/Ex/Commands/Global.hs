module Yi.Keymap.Vim.Ex.Commands.Global
    ( parse
    ) where

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.List (isInfixOf)
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer.Adjusted
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import qualified Yi.Keymap.Vim.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim.Ex.Commands.Substitute as Substitute

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try (P.string "global/") <|> P.string "g/"
    predicate <- P.many (P.noneOf "/")
    void $ P.char '/'
    cmdString <- P.many P.anyChar
    cmd <- case stringToExCommand allowedCmds cmdString of
            Just c -> return c
            _ -> fail "Unexpected command argument for global command."
    return $! global predicate cmd

global :: String -> ExCommand -> ExCommand
global p c = Common.pureExCommand {
    cmdShow = concat ["g/", p, "/", show c]
  , cmdAction = EditorA $ do
        mark <- withBuffer0 setMarkHereB
        lineCount <- withBuffer0 lineCountB
        forM_ (reverse [1..lineCount]) $ \l -> do
            ln <- withBuffer0 $ gotoLn l >> readLnB
            when (p `isInfixOf` ln) $
                case cmdAction c of
                    BufferA action -> withBuffer0 $ void action
                    EditorA action -> void action
                    _ -> error "Impure command as an argument to global."
        withBuffer0 $ do
            use (markPointA mark) >>= moveTo
            deleteMarkB mark
  }

allowedCmds :: [String -> Maybe ExCommand]
allowedCmds = [Delete.parse, Substitute.parse]

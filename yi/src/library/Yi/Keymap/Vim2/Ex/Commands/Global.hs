module Yi.Keymap.Vim2.Ex.Commands.Global
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import Data.List (reverse, isInfixOf)
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common
import qualified Yi.Keymap.Vim2.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim2.Ex.Commands.Substitute as Substitute

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    discard $ P.try (P.string "global/") <|> P.string "g/"
    predicate <- P.many (P.noneOf "/")
    discard $ P.char '/'
    cmdString <- P.many P.anyChar
    cmd <- case stringToExCommand allowedCmds cmdString of
            Just c -> return c
            _ -> fail "Unexpected command argument for global command."
    return $! global predicate cmd

global :: String -> ExCommand -> ExCommand
global p c = Common.pureExCommand {
    cmdShow = concat ["g/", p, "/", show c]
  , cmdAction = Left $ do
        mark <- withBuffer0 setMarkHereB
        lineCount <- withBuffer0 lineCountB
        forM_ (reverse [1..lineCount]) $ \l -> do
            ln <- withBuffer0 $ gotoLn l >> readLnB
            when (p `isInfixOf` ln) $
                case cmdAction c of
                    Left action -> action
                    _ -> error "Impure command as an argument to global."
        withBuffer0 $ do
            getMarkPointB mark >>= moveTo
            deleteMarkB mark
  }

allowedCmds :: [String -> Maybe ExCommand]
allowedCmds = [Delete.parse, Substitute.parse]
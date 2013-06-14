module Yi.Keymap.Vim2.Ex.Commands.Global
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import Data.List (reverse, isInfixOf)
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common
import qualified Yi.Keymap.Vim2.Ex.Commands.Delete as Delete
import qualified Yi.Keymap.Vim2.Ex.Commands.Substitute as Substitute

commands :: [ExCommandBox]
commands = [ pack $ Global undefined undefined ]

data Global = Global {
    _predicate :: !String
  , _command :: !ExCommandBox
}

instance Show Global where
    show (Global p c) = concat ["g/", p, "/", show c]

instance ExCommand Global where
    cmdIsPure _ = True
    cmdParse _ = parse $ do
        discard $ P.try (P.string "global/") <|> P.string "g/"
        predicate <- P.many (P.noneOf "/")
        discard $ P.char '/'
        cmdString <- P.many P.anyChar
        cmd <- case stringToExCommand allowedCmds cmdString of
                Just c -> return c
                _ -> fail "Unexpected command argument for global command."
        return $! Global predicate cmd
    cmdAction (Global p c) = Left $ do
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

allowedCmds :: [ExCommandBox]
allowedCmds = concat [Delete.commands, Substitute.commands]
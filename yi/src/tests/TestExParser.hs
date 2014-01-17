module TestExParser (getTestGroup) where

import Control.Applicative
import Data.Maybe

import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Test.Framework as TF
import Yi.Keymap.Vim2.Ex
import qualified Yi.Keymap.Vim2.Ex.Commands.Buffer as Buffer
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Buffers as Buffers
import qualified Yi.Keymap.Vim2.Ex.Commands.BufferDelete as BufferDelete
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Cabal as Cabal
import qualified Yi.Keymap.Vim2.Ex.Commands.Delete as Delete
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Edit as Edit
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Global as Global
-- import qualified Yi.Keymap.Vim2.Ex.Commands.GotoLine as GotoLine
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Nohl as Nohl
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Paste as Paste
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Quit as Quit
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Reload as Reload
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Substitute as Substitute
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Write as Write
-- import qualified Yi.Keymap.Vim2.Ex.Commands.Yi as Yi


-- fullCommands :: [String]
-- fullCommands =
--     [ "delete"
--     , "quit"
--     , "quit!"
--     , "quitall"
--     , "quitall!"
--     , "wquit"
--     , "wquit!"
--     , "wquitall!"
--     , "edit foo"
--     , "tabedit foo"
--     , "write foo"
--     , "write"
--     , "writeall"
--     ]

type CommandDescription = String


-- TODO Generate these strings with QuickCheck of SmallCheck.
stringToCommands :: [([String], (String -> Maybe ExCommand), CommandDescription)]
stringToCommands =
    [ ([ "buffer", "buf", "b"
       , "buffer ", "buf ", "b "
       , "buffer bob", "buf bob", "b bob"
       , "buffer 8", "buf 8", "b 8"
       , "buffer8", "buf8", "b8"
       , "buffer!", "buf!", "b!"
       , "buffer bob!", "buf bob!", "b bob!"
       , "buffer! 8", "buf! 8", "b! 8"
       , "buffer!8", "buf!8", "b!8"
       ], Buffer.parse, "Buffer.parse")

    , (["bdelete", "bdel", "bd"], BufferDelete.parse, "BufferDelete.parse")
    , (["delete", "del", "d"], Delete.parse, "Delete.parse")
    ]

-- | Checks that the expected 'ExCommand' parser parses the command string.
--
-- This check is performed in isolation of all other 'ExCommand' parsers, so it
-- doesn't check that an unexpected parser won't also parse the command and
-- be selected in preference.
expectedParsersParse :: [String]
           -> (String -> Maybe ExCommand)
           -> CommandDescription
           -> TF.Test
expectedParsersParse ss commandParser descr =
    TF.testGroup descr $
        map (expectedParserParses commandParser) ss
  where
    expectedParserParses :: (String -> Maybe ExCommand)
                         -> String
                         -> TF.Test
    expectedParserParses commandParser s =
        testCase ("Parses \"" ++ s ++ "\"") $
            assertJust "" (stringToExCommand [commandParser] s)


-- | Checks that the expected 'ExCommand' will be selected from all default 'ExCommand's.
--
-- Silently fails if no ExCommand parses the string on the assumption that
-- 'checkExpectedParserParses' will report the error.
expectedParsersSelected :: [String]
                -> (String -> Maybe ExCommand)
                -> CommandDescription
                -> TF.Test
expectedParsersSelected ss commandParser descr =
    TF.testGroup descr $
        map (expectedParserSelected commandParser) ss
  where
    expectedParserSelected :: (String -> Maybe ExCommand)
                           -> String
                           -> TF.Test
    expectedParserSelected expectedCommandParser s =
        let expectedName = expectedCommandName
            actualName   = actualCommandName
        in
            testCase ("Selected for \"" ++ s ++ "\"") $
                assertEqual "" expectedName actualName
      where
        expectedCommandName = commandNameFor [expectedCommandParser]
        actualCommandName   = commandNameFor defExCommandParsers
        commandNameFor parsers =
            cmdShow <$> stringToExCommand parsers s


assertJust msg a = assertBool msg (isJust a)


-- | Tests for the Ex command parsers in the Vim2 Keymap.
--
-- Tests that the parsers parse the strings they are expected to and that
-- the expected parser is selected for string.
--
-- The actions of the ex commands are not tested here.
getTestGroup :: TF.Test
getTestGroup = TF.testGroup "Vim2 Keymap Ex command parsers" [
                   TF.testGroup "Expected parser parses" $
                       map (uncurry3 expectedParsersParse) stringToCommands
                 , TF.testGroup "Expected parser selected" $
                       map (uncurry3 expectedParsersSelected) stringToCommands
                 ]
  where
    uncurry3 f (a,b,c) = f a b c

{-# LANGUAGE OverloadedStrings #-}
module Vim.TestExCommandParsers (tests) where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex
import qualified Yi.Keymap.Vim.Ex.Commands.Buffer as Buffer
import qualified Yi.Keymap.Vim.Ex.Commands.BufferDelete as BufferDelete
import qualified Yi.Keymap.Vim.Ex.Commands.Delete as Delete

data CommandParser = CommandParser
    { cpDescription :: String
    , cpParser      :: String -> Maybe ExCommand
    , cpNames       :: [String]
    , cpAcceptsBang :: Bool
    , cpAcceptsCount :: Bool
    , cpArgs        :: Gen String
    }

addingSpace :: Gen String -> Gen String
addingSpace = fmap (" " <>)

numberString :: Gen String
numberString = (\(NonNegative n) -> show n) <$> (arbitrary :: Gen (NonNegative Int))

-- | QuickCheck Generator of buffer identifiers.
--
-- A buffer identifier is either an empty string, a "%" character, a "#"
-- character, a string containing only numbers (optionally preceeded by
-- a space), or a string containing any chars preceeded by a space. E.g.,
--
--   ["", "%", "#", " myBufferName", " 45", "45"]
--
-- TODO Don't select "", "%", "#" half of the time.
bufferIdentifier :: Gen String
bufferIdentifier =
    oneof [ addingSpace arbitrary
          , addingSpace numberString
          , numberString
          , oneof [pure "%", pure " %"]
          , oneof [pure "#", pure " #"]
          , pure ""
          ]

-- | QuickCheck generator of strings suitable for use as register names in Vim
-- ex command lines. Does not include a preceding @"@.
registerName :: Gen String
registerName =
    (:[]) <$> oneof [ elements ['0'..'9']
                    , elements ['a'..'z']
                    , elements ['A'..'Z']
                    , elements ['"', '-', '=', '*', '+', '~', '_', '/']
                    -- TODO Should the read-only registers be included here?
                    -- , element [':', '.', '%', '#']
                    ]

-- | QuickCheck generator of strings suitable for use as counts in Vim ex
-- command lines
count :: Gen String
count = numberString

commandParsers :: [CommandParser]
commandParsers =
    [ CommandParser
          "Buffer.parse"
          (Buffer.parse . Ev . T.pack)
          ["buffer", "buf", "bu", "b"]
          True
          True
          bufferIdentifier

    , CommandParser
          "BufferDelete.parse"
          (BufferDelete.parse . Ev . T.pack)
          ["bdelete", "bdel", "bd"]
          True
          False
          (unwords <$> listOf bufferIdentifier)

    , CommandParser
          "Delete.parse"
          (Delete.parse . Ev . T.pack)
          ["delete", "del", "de", "d"]
          -- XXX TODO support these weird abbreviations too?
          -- :dl, :dell, :delel, :deletl, :deletel
          -- :dp, :dep, :delp, :delep, :deletp, :deletep
          True
          False
          (oneof [ pure ""
                 , addingSpace registerName
                 , addingSpace count
                 , (<>) <$> addingSpace registerName <*> addingSpace count
                 ])
    ]


commandString :: CommandParser -> Gen String
commandString cp = do
    name <- elements $ cpNames cp
    bang <- if cpAcceptsBang cp
                then elements ["!", ""]
                else pure ""
    count' <- if cpAcceptsCount cp
                then count
                else pure ""
    args <- cpArgs cp
    return $ concat [count', name, bang, args]


expectedParserParses :: CommandParser -> TestTree
expectedParserParses commandParser =
    testProperty (cpDescription commandParser <> " parses expected input") $
        forAll (commandString commandParser)
               (isJust . cpParser commandParser)


expectedParserSelected :: CommandParser -> TestTree
expectedParserSelected expectedCommandParser =
    testProperty testName $
        forAll (commandString expectedCommandParser) $ \s ->
            let expectedName = expectedCommandName (Ev $ T.pack s)
                actualName   = actualCommandName (Ev $ T.pack s)
            in counterexample (errorMessage s actualName)
                              (expectedName == actualName)
  where
    unE = T.unpack . _unEv
    expectedCommandName = commandNameFor [cpParser expectedCommandParser . unE]
    actualCommandName   = commandNameFor defExCommandParsers
    commandNameFor parsers s =
        cmdShow <$> evStringToExCommand parsers s
    errorMessage s actualName =
        "Parsed " <> show s <> " to " <> show actualName <> " command"
    testName =
       cpDescription expectedCommandParser <> " selected for expected input"



-- | Tests for the Ex command parsers in the Vim Keymap.
--
-- Tests that the parsers parse the strings they are expected to and that
-- the expected parser is selected for string.
--
-- The actions of the ex commands are not tested here.
tests :: TestTree
tests =
    testGroup "Vim keymap ex command parsers"
        [ testGroup "Expected parser parses" $
              map expectedParserParses commandParsers
        , testGroup "Expected parser selected" $
              map expectedParserSelected commandParsers
        ]

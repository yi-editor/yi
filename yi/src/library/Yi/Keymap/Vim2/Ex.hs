module Yi.Keymap.Vim2.Ex
    ( exEvalE
    , exEvalY
    -- for testing purposes:
    , ExCommand(..)
    , ExReplaceFlag(..)
    , stringToExCommand
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either (either)
import Data.List (reverse)
import Data.Maybe (maybe)

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer
import Yi.Editor
import Yi.Keymap
import Yi.Search (searchAndRepRegion0, makeSimpleSearch)

data ExReplaceFlag = Global
                   | CaseInsensitive
                   | EveryLine
    deriving (Show, Eq)

flagFromChar :: Char -> ExReplaceFlag
flagFromChar 'g' = Global
flagFromChar 'i' = CaseInsensitive
flagFromChar '%' = EveryLine

data ExCommand = ExGlobal String Bool ExCommand
               | ExDelete
               | ExReplace String String [ExReplaceFlag]
               | ExGotoLine Int
    deriving (Show, Eq)

parseExCommand :: P.GenParser Char st ExCommand
parseExCommand = P.choice
    [ parseExGlobal
    , parseExDelete
    , parseExReplace
    , parseExGoto
    ]

parseExDelete :: P.GenParser Char st ExCommand
parseExDelete = do
    P.try ( P.string "delete") <|> P.string "d"
    return ExDelete

parseExGlobal :: P.GenParser Char st ExCommand
parseExGlobal = do
    vs <- P.many (P.char 'v')
    P.try (P.string "global") <|> (P.string "g")
    bangs <- P.many (P.char '!')
    P.char '/'
    term <- P.many (P.noneOf "/")
    P.char '/'
    cmd <- parseExCommand
    return $ ExGlobal term ((null bangs) /= (null vs)) cmd

parseExReplace :: P.GenParser Char st ExCommand
parseExReplace = do
    percents <- P.many (P.char '%')
    P.string "s/"
    from <- P.many (P.noneOf "/")
    P.char '/'
    to <- P.many (P.noneOf "/")
    P.char '/'
    flagChars <- P.many (P.oneOf "gi")
    let flags = fmap flagFromChar (flagChars ++ percents)
    return $ ExReplace from to flags

parseExGoto :: P.GenParser Char st ExCommand
parseExGoto = do
    ds <- P.many1 P.digit
    return $ ExGotoLine (read ds)

executeExCommand :: ExCommand -> EditorM ()
executeExCommand (ExReplace from to flags) = withBuffer0 $ do
    let regex = makeSimpleSearch from
        replace = do
            region <- regionOfB Line
            discard $ searchAndRepRegion0 regex to (Global `elem` flags) region

    if EveryLine `elem` flags
    then withEveryLineB replace
    else replace

    moveToSol
executeExCommand (ExGotoLine l) = discard $ withBuffer0 $ gotoLn l
executeExCommand ExDelete =
    withBuffer0 $ do
        deleteUnitB Line Forward
        deleteN 1
executeExCommand (ExGlobal term _ cmd) = do
    mark <- withBuffer0 setMarkHereB
    lineCount <- withBuffer0 lineCountB
    forM_ (reverse [1..lineCount]) $ \l -> do
        withBuffer0 $ gotoLn l
        executeExCommand cmd
    withBuffer0 $ do
        getMarkPointB mark >>= moveTo
        deleteMarkB mark

stringToExCommand :: String -> Either P.ParseError ExCommand
stringToExCommand = P.parse parseExCommand ""

exEvalE :: String -> EditorM ()
exEvalE command =
    either
    (const $ return ())
    executeExCommand
    (stringToExCommand command)

exEvalY :: String -> YiM ()
exEvalY command = return ()

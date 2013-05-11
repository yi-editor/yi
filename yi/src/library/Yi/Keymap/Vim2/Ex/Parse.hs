module Yi.Keymap.Vim2.Ex.Parse
    ( stringToExCommand
    ) where

import Prelude ()
import Yi.Prelude

import Data.Either

import Yi.Keymap.Vim2.Ex.Types

import qualified Text.ParserCombinators.Parsec as P

stringToExCommand :: String -> Either P.ParseError ExCommand
stringToExCommand = P.parse parseExCommand ""

flagFromChar :: Char -> ExReplaceFlag
flagFromChar 'g' = Global
flagFromChar 'i' = CaseInsensitive
flagFromChar '%' = EveryLine

parseExCommand :: P.GenParser Char st ExCommand
parseExCommand = P.choice
    [ ExImpure <$> parseImpureCommand
    , ExPure <$> parsePureCommand
    ]

parseImpureCommand :: P.GenParser Char st ExImpureCommand
parseImpureCommand = P.choice
    [ parseQuit
    , parseOpenFile
    ]

parsePureCommand :: P.GenParser Char st ExPureCommand
parsePureCommand = P.choice
    [ parseGlobal
    , parseDelete
    , parseReplace
    , parseGoto
    ]

parseQuit :: P.GenParser Char st ExImpureCommand
parseQuit = do
    P.try ( P.string "quit") <|> P.string "q"
    as <- P.many (P.char 'a')
    bangs <- P.many (P.char '!')

    if null as
    then return $ ExQuit (not (null bangs))
    else return $ ExQuitAll (not (null bangs))

parseOpenFile :: P.GenParser Char st ExImpureCommand
parseOpenFile = do
    P.try ( P.string "edit") <|> P.string "e"
    P.many1 P.space
    filename <- P.many1 P.anyChar
    return $ ExOpenFile filename

parseDelete :: P.GenParser Char st ExPureCommand
parseDelete = do
    P.try ( P.string "delete") <|> P.string "d"
    return ExDelete

parseGlobal :: P.GenParser Char st ExPureCommand
parseGlobal = do
    vs <- P.many (P.char 'v')
    P.try (P.string "global") <|> P.string "g"
    bangs <- P.many (P.char '!')
    P.char '/'
    term <- P.many (P.noneOf "/")
    P.char '/'
    cmd <- parsePureCommand
    return $ ExGlobal term (null bangs /= null vs) cmd

parseReplace :: P.GenParser Char st ExPureCommand
parseReplace = do
    percents <- P.many (P.char '%')
    P.try (P.string "substitute/") <|> P.string "s/"
    from <- P.many (P.noneOf "/")
    P.char '/'
    to <- P.many (P.noneOf "/")
    P.char '/'
    flagChars <- P.many (P.oneOf "gi")
    let flags = fmap flagFromChar (flagChars ++ percents)
    return $ ExReplace from to flags

parseGoto :: P.GenParser Char st ExPureCommand
parseGoto = do
    ds <- P.many1 P.digit
    return $ ExGotoLine (read ds)


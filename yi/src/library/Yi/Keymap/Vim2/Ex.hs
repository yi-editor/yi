module Yi.Keymap.Vim2.Ex
    ( exEvalE
    , exEvalY
    ) where

import Prelude ()
import Yi.Prelude

import Data.Maybe (maybe)
import Data.Either (either)

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

data ExCommand = ExReplace String String [ExReplaceFlag]
               | ExGotoLine Int
    deriving Show

parseExCommand :: P.GenParser Char st ExCommand
parseExCommand = P.choice
    [ parseExReplace
    , parseExGoto
    ]

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

exEvalE :: String -> EditorM ()
exEvalE command =
    either
    (const $ return ())
    executeExCommand
    (P.parse parseExCommand "" command)

exEvalY :: String -> YiM ()
exEvalY command = return ()

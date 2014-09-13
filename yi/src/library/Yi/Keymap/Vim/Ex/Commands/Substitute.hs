module Yi.Keymap.Vim.Ex.Commands.Substitute
    ( parse
    ) where

import Control.Applicative
import Control.Monad

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer.Adjusted hiding (Delete)
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Yi.Search

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    percents <- P.many (P.char '%')
    void $ P.try (P.string "substitute") <|> P.string "s"
    delimiter <- P.oneOf "!@#$%^&*()[]{}<>/.,~';:?-="
    from <- P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    to <- P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    flagChars <- P.many (P.oneOf "gi")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        (not $ null percents)

substitute :: String -> String -> Char -> Bool -> Bool -> Bool -> ExCommand
substitute from to delimiter global caseInsensitive allLines = Common.pureExCommand {
    cmdShow = concat
        [ if allLines then "%" else ""
        , "substitute" , delimiter : from , delimiter : to , [delimiter]
        , if caseInsensitive then "i" else ""
        , if global then "g" else ""
        ]
  , cmdAction = BufferA $ do
        let regex = makeSimpleSearch from
            replace = do
                region <- regionOfB Line
                void $ searchAndRepRegion0 regex to global region

        if allLines
        then withEveryLineB replace
        else replace

        moveToSol
  }

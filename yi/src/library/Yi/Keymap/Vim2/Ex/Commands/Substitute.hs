module Yi.Keymap.Vim2.Ex.Commands.Substitute
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common
import Yi.Search

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    percents <- P.many (P.char '%')
    discard $ P.try (P.string "substitute/") <|> P.string "s/"
    from <- P.many (P.noneOf "/")
    discard $ P.char '/'
    to <- P.many (P.noneOf "/")
    discard $ P.char '/'
    flagChars <- P.many (P.oneOf "gi")
    return $! substitute from to
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        (not $ null percents)

substitute :: String -> String -> Bool -> Bool -> Bool -> ExCommand
substitute from to global caseInsensitive allLines = Common.pureExCommand {
    cmdShow = concat
        [ if allLines then "%" else ""
        , "substitute/" , from , "/" , to , "/"
        , if caseInsensitive then "i" else ""
        , if global then "g" else ""
        ]
  , cmdAction = BufferA $ do
        let regex = makeSimpleSearch from
            replace = do
                region <- regionOfB Line
                discard $ searchAndRepRegion0 regex to global region

        if allLines
        then withEveryLineB replace
        else replace

        moveToSol
  }
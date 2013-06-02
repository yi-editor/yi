module Yi.Keymap.Vim2.Ex.Commands.Substitute
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common
import Yi.Search

commands :: [ExCommandBox]
commands = [pack $ Substitute undefined undefined undefined undefined undefined]

data Substitute = Substitute {
        _subFrom :: !String
      , _subTo :: !String
      , _subGlobally :: !Bool
      , _subCaseInsensitive :: !Bool
      , _subAllLines :: !Bool
    }
    deriving Show

instance ExCommand Substitute where
    cmdIsPure _ = True
    cmdComplete _ = return Nothing
    cmdParse _ = parse $ do
        percents <- P.many (P.char '%')
        discard $ P.try (P.string "substitute/") <|> P.string "s/"
        from <- P.many (P.noneOf "/")
        discard $ P.char '/'
        to <- P.many (P.noneOf "/")
        discard $ P.char '/'
        flagChars <- P.many (P.oneOf "gi")
        return $! Substitute from to
            ('g' `elem` flagChars)
            ('i' `elem` flagChars)
            (not $ null percents)
    cmdAction (Substitute from to global _insensitive allLines) =
        Left . withBuffer0 $ do
            let regex = makeSimpleSearch from
                replace = do
                    region <- regionOfB Line
                    discard $ searchAndRepRegion0 regex to global region

            if allLines
            then withEveryLineB replace
            else replace

            moveToSol

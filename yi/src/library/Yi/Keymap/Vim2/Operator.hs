module Yi.Keymap.Vim2.Operator
    ( VimOperator(..)
    , operators
    , delete
    , change
    , yank
    , stringToOperator
    , mkCharTransformOperator
    , operatorApplyToTextObjectE
    , lastCharForOperator
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (toLower, toUpper)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.TextObject
import Yi.Misc

data VimOperator = VimOperator {
    operatorName :: !OperatorName
  , operatorApplyToRegionE :: Int -> StyledRegion -> EditorM RepeatToken
}

operators :: [VimOperator]
operators =
    [ yank
    , delete
    , change
    , mkCharTransformOperator "gu" toLower
    , mkCharTransformOperator "gU" toUpper
    , mkCharTransformOperator "g~" switchCaseChar
    , mkCharTransformOperator "g?" rot13Char
    , mkShiftOperator ">" id
    , mkShiftOperator "<lt>" negate
    ]

stringToOperator :: [VimOperator] -> OperatorName -> Maybe VimOperator
stringToOperator ops name = find ((== name) . operatorName) ops

operatorApplyToTextObjectE :: VimOperator -> Int -> CountedTextObject -> EditorM RepeatToken
operatorApplyToTextObjectE op count cto = do
    styledRegion <- withBuffer0 $ regionOfTextObjectB cto
    operatorApplyToRegionE op count styledRegion

yank :: VimOperator
yank = VimOperator {
    operatorName = "y"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ moveTo (regionStart reg)
        switchModeE Normal
        return Finish
}

delete :: VimOperator
delete = VimOperator {
    operatorName = "d"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
            eof <- atEof
            if eof
            then do
                leftB
                c <- readB
                when (c == '\n') $ deleteN 1 >> moveToSol
            else leftOnEol
        switchModeE Normal
        return Finish
}

change :: VimOperator
change = VimOperator {
    operatorName = "c"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
        switchModeE $ Insert 'c'
        return Continue
}

mkCharTransformOperator :: OperatorName -> (Char -> Char) -> VimOperator
mkCharTransformOperator name f = VimOperator {
    operatorName = name
  , operatorApplyToRegionE = \count sreg -> do
        withBuffer0 $ transformCharactersInRegionB sreg
                    $ foldr (.) id (replicate count f)
        switchModeE Normal
        return Finish
}

mkShiftOperator :: OperatorName -> (Int -> Int) -> VimOperator
mkShiftOperator name countMod = VimOperator {
    operatorName = name
  , operatorApplyToRegionE = \count (StyledRegion style reg) -> do
        withBuffer0 $ shiftIndentOfRegion (countMod count) =<< convertRegionToStyleB reg style
        switchModeE Normal
        return Finish
}

lastCharForOperator :: VimOperator -> Char
lastCharForOperator = last . operatorName
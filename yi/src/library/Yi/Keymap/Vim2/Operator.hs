module Yi.Keymap.Vim2.Operator
    ( VimOperator(..)
    , operators
    , opDelete
    , opChange
    , opYank
    , opFormat
    , stringToOperator
    , mkCharTransformOperator
    , operatorApplyToTextObjectE
    , lastCharForOperator
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (toLower, toUpper)
import Data.List (isSuffixOf)

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
    [ opYank
    , opDelete
    , opChange
    , opFormat
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

opYank :: VimOperator
opYank = VimOperator {
    operatorName = "y"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
        withBuffer0 $ moveTo . regionStart =<< convertRegionToStyleB reg style
        switchModeE Normal
        return Finish
}

opDelete :: VimOperator
opDelete = VimOperator {
    operatorName = "d"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
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

opChange :: VimOperator
opChange = VimOperator {
    operatorName = "c"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
        withBuffer0 $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
        switchModeE $ Insert 'c'
        return Continue
}

opFormat :: VimOperator
opFormat = VimOperator {
    operatorName = "gq"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
      withBuffer0 $ formatRegionB style reg
      switchModeE Normal
      return Finish
}

formatRegionB :: RegionStyle -> Region -> BufferM ()
formatRegionB Block reg = return ()
formatRegionB style reg = do
    -- TODO: handle indentation
    -- TODO: break words
    let (start, end) = (regionStart reg, regionEnd reg)
    moveTo start
    let go = do
            rightB
            p <- pointB
            col <- curCol
            char <- readB
            if p >= end
            then return ()
            else if col < 80 && char == '\n'
            then writeB ' ' >> go
            else if col == 80 && char /= '\n'
            then writeB '\n' >> go
            else go
    go
    moveTo start

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

lastCharForOperator :: VimOperator -> String
lastCharForOperator (VimOperator { operatorName = name }) =
    if "<lt>" `isSuffixOf` name then "<lt>" else [last name]
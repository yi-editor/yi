{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Operator
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements some operators for the Vim keymap.

module Yi.Keymap.Vim.Operator
    ( VimOperator(..)
    , defOperators
    , opDelete
    , opChange
    , opYank
    , opFormat
    , stringToOperator
    , mkCharTransformOperator
    , operatorApplyToTextObjectE
    , lastCharForOperator
    ) where

import           Control.Monad
import           Data.Char (toLower, toUpper)
import           Data.Foldable (find)
import           Data.Monoid
import qualified Data.Text as T
import           Yi.Buffer.Adjusted hiding (Insert)
import           Yi.Editor
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.EventUtils
import           Yi.Keymap.Vim.StateUtils
import           Yi.Keymap.Vim.StyledRegion
import           Yi.Keymap.Vim.TextObject
import           Yi.Keymap.Vim.Utils
import           Yi.Misc

data VimOperator = VimOperator {
    operatorName :: !OperatorName
  , operatorApplyToRegionE :: Int -> StyledRegion -> EditorM RepeatToken
}

defOperators :: [VimOperator]
defOperators =
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
    styledRegion <- withCurrentBuffer $ regionOfTextObjectB cto
    operatorApplyToRegionE op count styledRegion

opYank :: VimOperator
opYank = VimOperator {
    operatorName = "y"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withCurrentBuffer $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
        withCurrentBuffer $ moveTo . regionStart =<< convertRegionToStyleB reg style
        switchModeE Normal
        return Finish
}

opDelete :: VimOperator
opDelete = VimOperator {
    operatorName = "d"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
        s <- withCurrentBuffer $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
        withCurrentBuffer $ do
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
        s <- withCurrentBuffer $ readRegionRopeWithStyleB reg style
        regName <- fmap vsActiveRegister getDynamic
        setRegisterE regName style s
        withCurrentBuffer $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
        switchModeE $ Insert 'c'
        return Continue
}

opFormat :: VimOperator
opFormat = VimOperator {
    operatorName = "gq"
  , operatorApplyToRegionE = \_count (StyledRegion style reg) -> do
      withCurrentBuffer $ formatRegionB style reg
      switchModeE Normal
      return Finish
}

formatRegionB :: RegionStyle -> Region -> BufferM ()
formatRegionB Block _reg = return ()
formatRegionB _style reg = do
    -- TODO: handle indentation
    -- TODO: break words
    let (start, end) = (regionStart reg, regionEnd reg)
    moveTo start
    let go = do
            rightB
            p <- pointB
            col <- curCol
            char <- readB
            unless (p >= end) $
                if col < 80 && char == '\n'
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
        withCurrentBuffer $ transformCharactersInRegionB sreg
                    $ foldr (.) id (replicate count f)
        switchModeE Normal
        return Finish
}

mkShiftOperator :: OperatorName -> (Int -> Int) -> VimOperator
mkShiftOperator name countMod = VimOperator {
    operatorName = name
  , operatorApplyToRegionE = \count (StyledRegion style reg) -> do
        withCurrentBuffer $
            if style == Block
            then indentBlockRegionB (countMod count) reg
            else do
                reg' <- convertRegionToStyleB reg style
                shiftIndentOfRegionB (countMod count) reg'
        switchModeE Normal
        return Finish
}

lastCharForOperator :: VimOperator -> String
lastCharForOperator (VimOperator { operatorName = name })
    -- This cast here seems stupid, maybe we should only have one
    -- type?
    = case parseEvents (Ev . _unOp $ name) of
        [] -> error $ "invalid operator name " <> T.unpack (_unOp name)
        evs -> T.unpack . _unEv . eventToEventString $ last evs

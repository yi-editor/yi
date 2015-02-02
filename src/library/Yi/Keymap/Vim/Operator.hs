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

import           Control.Applicative        ((<$>))
import           Control.Monad              (when)
import           Data.Char                  (isSpace, toLower, toUpper)
import           Data.Foldable              (find)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T (unpack)
import           Yi.Buffer.Adjusted         hiding (Insert)
import           Yi.Editor                  (EditorM, getEditorDyn, withCurrentBuffer)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.EventUtils   (eventToEventString, parseEvents)
import           Yi.Keymap.Vim.StateUtils   (setRegisterE, switchModeE)
import           Yi.Keymap.Vim.StyledRegion (StyledRegion (..), transformCharactersInRegionB)
import           Yi.Keymap.Vim.TextObject   (CountedTextObject, regionOfTextObjectB)
import           Yi.Keymap.Vim.Utils        (indentBlockRegionB)
import           Yi.Misc                    (rot13Char)
import           Yi.Rope                    (YiString)
import qualified Yi.Rope                    as R

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
        regName <- fmap vsActiveRegister getEditorDyn
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
        regName <- fmap vsActiveRegister getEditorDyn
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
        regName <- fmap vsActiveRegister getEditorDyn
        setRegisterE regName style s
        withCurrentBuffer $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
            when (style == LineWise) $ do
                insertB '\n'
                leftB
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
    start <- solPointB $ regionStart reg
    end <- eolPointB $ regionEnd reg
    moveTo start
    -- Don't use firstNonSpaceB since paragraphs can start with lines made
    -- completely of whitespace (which should be fixed)
    untilB_ ((not . isSpace) <$> readB) rightB
    indent <- curCol
    modifyRegionB (formatStringWithIndent indent) $ reg { regionStart = start
                                                        , regionEnd = end
                                                        }
    -- Emulate vim behaviour
    moveTo =<< solPointB end
    firstNonSpaceB

formatStringWithIndent :: Int -> YiString -> YiString
formatStringWithIndent indent str
    | R.null str = R.empty
    | otherwise = let spaces = R.replicateChar indent ' '
                      (formattedLine, textToFormat) = getNextLine (80 - indent) str
                      lineEnd = if R.null textToFormat
                                then R.empty
                                else '\n' `R.cons` formatStringWithIndent indent textToFormat
                  in R.concat [ spaces
                              , formattedLine
                              , lineEnd
                              ]

getNextLine :: Int -> YiString -> (YiString, YiString)
getNextLine maxLength str = let firstSplit = takeBlock (R.empty, R.dropWhile isSpace str)
                                isMaxLength (l, r) = R.length l > maxLength || R.null r
                            in if isMaxLength firstSplit
                               then firstSplit
                               else let (line, remainingText) = until isMaxLength
                                                                      takeBlock
                                                                      firstSplit
                                    in if R.length line <= maxLength
                                       then (R.dropWhileEnd isSpace line, remainingText)
                                       else let (beginL, endL) = breakAtLastItem line
                                            in if isSpace $ fromJust $ R.head endL
                                               then (beginL, remainingText)
                                               else (R.dropWhileEnd isSpace beginL, endL `R.append` remainingText)
                            where
                                isMatch (Just x) y = isSpace x == isSpace y
                                isMatch Nothing _ = False

                                -- Gets the next block of either whitespace, or non-whitespace,
                                -- characters
                                takeBlock (cur, rest) =
                                    let (word, line) = R.span (isMatch $ R.head rest) rest
                                    in (cur `R.append` R.map (\c -> if c == '\n' then ' ' else c) word, line)
                                breakAtLastItem s =
                                    let y = R.takeWhileEnd (isMatch $ R.last s) s
                                        (x, _) = R.splitAt (R.length s - R.length y) s
                                    in (x, y)

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

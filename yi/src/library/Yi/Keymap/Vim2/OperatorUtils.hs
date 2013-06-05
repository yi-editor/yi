module Yi.Keymap.Vim2.OperatorUtils
    ( applyOperatorToTextObjectE
    , applyOperatorToRegionE
    , lastCharForOperator
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.StyledRegion
import Yi.Keymap.Vim2.TextObject
import Yi.Misc

applyOperatorToTextObjectE :: Int -> VimOperator -> CountedTextObject -> EditorM ()
applyOperatorToTextObjectE count op cto = do
    styledRegion <- withBuffer0 $ regionOfTextObjectB cto
    applyOperatorToRegionE count op styledRegion

applyOperatorToRegionE :: Int -> VimOperator -> StyledRegion -> EditorM ()
applyOperatorToRegionE count op sreg@(StyledRegion style reg) = case op of
    OpDelete -> do
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
    OpChange -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ do
            point <- deleteRegionWithStyleB reg style
            moveTo point
        switchModeE $ Insert 'c'
    OpLowerCase -> withBuffer0 $ transformCharactersInRegionB sreg toLower
    OpUpperCase -> withBuffer0 $ transformCharactersInRegionB sreg toUpper
    OpSwitchCase -> withBuffer0 $ transformCharactersInRegionB sreg switchCaseChar
    OpRot13 -> withBuffer0 $ transformCharactersInRegionB sreg rot13Char
    OpYank -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ moveTo (regionStart reg)
    OpShiftRight -> shiftRight count reg style
    OpShiftLeft -> shiftLeft count reg style
    _ -> withBuffer0 $ insertN $ "Operator not supported " ++ show op

shiftRight, shiftLeft :: Int -> Region -> RegionStyle -> EditorM ()
shiftRight count reg style =
    withBuffer0 $ shiftIndentOfRegion count =<< convertRegionToStyleB reg style
shiftLeft count reg style = shiftRight (negate count) reg style

-- TODO eliminate redundancy
lastCharForOperator :: VimOperator -> Char
lastCharForOperator op = fromJust $ lookup op
    [ (OpYank, 'y')
    , (OpReindent, '=')
    , (OpShiftRight, '>')
    , (OpShiftLeft, '<')
    , (OpFormat, 'q')
    , (OpDelete, 'd')
    , (OpChange, 'c')
    , (OpLowerCase, 'u')
    , (OpUpperCase, 'U')
    , (OpSwitchCase, '~')
    , (OpRot13, '?')
    ]

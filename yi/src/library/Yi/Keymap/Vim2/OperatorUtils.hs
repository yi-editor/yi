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
import Yi.Keymap.Vim2.TextObject
import Yi.Misc

applyOperatorToTextObjectE :: VimOperator -> TextObject -> EditorM ()
applyOperatorToTextObjectE op to = do
    styledRegion <- withBuffer0 $ regionOfTextObjectB to
    applyOperatorToRegionE op styledRegion

applyOperatorToRegionE :: VimOperator -> StyledRegion -> EditorM ()
applyOperatorToRegionE op (StyledRegion style reg) = case op of
    OpDelete -> do
        s <- withBuffer0 $ readRegionB' reg
        setDefaultRegisterE style s
        withBuffer0 $ deleteRegionB reg >> moveTo (regionStart reg) >> leftOnEol
    OpChange -> do
        s <- withBuffer0 $ readRegionB' reg
        setDefaultRegisterE style s
        withBuffer0 $ deleteRegionB reg >> moveTo (regionStart reg)
        switchModeE Insert
    OpLowerCase -> withBuffer0 $ transformCharactersInRegionB reg toLower
    OpUpperCase -> withBuffer0 $ transformCharactersInRegionB reg toUpper
    OpSwitchCase -> withBuffer0 $ transformCharactersInRegionB reg switchCaseChar
    OpRot13 -> withBuffer0 $ transformCharactersInRegionB reg rot13Char
    _ -> withBuffer0 $ insertN $ "Operator not supported " ++ show op

transformCharactersInRegionB :: Region -> (Char -> Char) -> BufferM ()
transformCharactersInRegionB reg f = savingPointB $ do
    s <- readRegionB reg
    replaceRegionB reg (fmap f s)

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

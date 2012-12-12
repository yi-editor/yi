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

applyOperatorToTextObjectE :: VimOperator -> CountedTextObject -> EditorM ()
applyOperatorToTextObjectE op cto = do
    styledRegion <- withBuffer0 $ regionOfTextObjectB cto
    applyOperatorToRegionE op styledRegion

applyOperatorToRegionE :: VimOperator -> StyledRegion -> EditorM ()
applyOperatorToRegionE op sreg@(StyledRegion style reg) = case op of
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
        switchModeE Insert
    OpLowerCase -> withBuffer0 $ transformCharactersInRegionB sreg toLower
    OpUpperCase -> withBuffer0 $ transformCharactersInRegionB sreg toUpper
    OpSwitchCase -> withBuffer0 $ transformCharactersInRegionB sreg switchCaseChar
    OpRot13 -> withBuffer0 $ transformCharactersInRegionB sreg rot13Char
    OpYank -> do
        s <- withBuffer0 $ readRegionRopeWithStyleB reg style
        setDefaultRegisterE style s
        withBuffer0 $ moveTo (regionStart reg)
    _ -> withBuffer0 $ insertN $ "Operator not supported " ++ show op

transformCharactersInRegionB :: StyledRegion -> (Char -> Char) -> BufferM ()
transformCharactersInRegionB (StyledRegion Block reg) f = do
    subregions <- splitBlockRegionToContiguousSubRegionsB reg
    forM_ subregions $ \sr ->
        transformCharactersInRegionB (StyledRegion Exclusive sr) f
    case subregions of
        (sr:_) -> moveTo (regionStart sr)
        [] -> error "Should never happen"
transformCharactersInRegionB (StyledRegion style reg) f = do
    reg' <- convertRegionToStyleB reg style
    s <- readRegionB reg'
    replaceRegionB reg' (fmap f s)
    moveTo (regionStart reg')

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

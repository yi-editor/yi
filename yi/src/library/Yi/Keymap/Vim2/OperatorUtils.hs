module Yi.Keymap.Vim2.OperatorUtils
    ( applyOperatorToTextObjectE
    , applyOperatorToRegionE
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (toLower, toUpper)
import qualified Data.Rope as R

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.TextObject
import Yi.Misc

applyOperatorToTextObjectE :: VimOperator -> Int -> TextObject -> EditorM ()
applyOperatorToTextObjectE op count to = do
    reg <- withBuffer0 $ textObjectRegionB count to
    applyOperatorToRegionE op reg

applyOperatorToRegionE :: VimOperator -> Region -> EditorM ()
applyOperatorToRegionE op reg = case op of
    OpDelete -> do
        s <- withBuffer0 $ readRegionB reg
        setDefaultRegisterE $ R.fromString s
        withBuffer0 $ deleteRegionB reg
    OpLowerCase -> withBuffer0 $ transformCharactersInRegionB reg toLower
    OpUpperCase -> withBuffer0 $ transformCharactersInRegionB reg toUpper
    OpSwitchCase -> withBuffer0 $ transformCharactersInRegionB reg switchCaseChar
    OpRot13 -> withBuffer0 $ transformCharactersInRegionB reg rot13Char
    _ -> withBuffer0 $ insertN $ "Operator not supported " ++ show op

transformCharactersInRegionB :: Region -> (Char -> Char) -> BufferM ()
transformCharactersInRegionB reg f = savingPointB $ do
    s <- readRegionB reg
    replaceRegionB reg (fmap f s)

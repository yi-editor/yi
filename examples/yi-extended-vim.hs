import Yi
import Yi.Keymap.Vim
import Yi.Buffer.Indent (indentAsPreviousB)
import Yi.Keymap.Keys
import Yi.Prelude
import Prelude ()
import qualified Yi.UI.Vty

main :: IO ()
main = yi $ defaultConfig 
    {
        defaultKm = mkKeymap extendedVimKeymap,
        debugMode = True
    }

extendedVimKeymap self = super
    {
        v_ins_char = 
            (deprioritize $ v_ins_char super) 
            -- On enter I always want to use the indent of previous line
            <|> ( spec KEnter ?>>! do
                    insertB '\n'
                    indentAsPreviousB
                )
            -- On starting to write a Haskell block comment I want the close comment text
            -- inserted automatically.
            <|> ( pString "{-" >>! do
                    insertN "{-  -}" 
                    leftN 3
                )
    }
    where super = defKeymap self


import Yi
import Yi.Keymap.Emacs as Emacs
import Yi.String (mapLines)

increaseIndent :: BufferM ()
increaseIndent = do
  r <- getSelectRegionB
  r' <- unitWiseRegion Line r -- extend the region to full lines.
  modifyRegionB (mapLines (' ':)) r'


main :: IO ()
main = yi $ defaultConfig
       {
         defaultKm = Emacs.keymap <|>
                       (ctrl (char '>') ?>>! increaseIndent)
         -- bind the function to Ctrl-> and mix with default Emacs keymap.
       }

import Yi
import Yi.Keymap.Emacs as Emacs
import Yi.String (mapLines)

increaseIndent :: BufferM ()
increaseIndent = do
   r <- getSelectRegionB
   r' <- unitWiseRegion Line r -- extend the region to full lines.
   modifyRegionB (mapLines (' ':)) r'


main :: IO ()
main = yi $ defaultEmacsConfig
  { defaultKm =
      Emacs.mkKeymap $ override Emacs.defKeymap $ \parent _self ->
        parent {
           eKeymap = (eKeymap parent) ||> (ctrlCh 'w' ?>>! bkillWordB)
				      ||> (metaCh 'z' ?>>! undoB)
        }
      -- bind M-> to increaseIndent and mix with default Emacs keymap.
  }

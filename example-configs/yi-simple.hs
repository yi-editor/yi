import           Yi
import qualified Yi.Rope as R
import           Yi.Keymap.Emacs as Emacs
import           Yi.String (mapLines)

increaseIndent :: BufferM ()
increaseIndent = do
   r <- getSelectRegionB
   r' <- unitWiseRegion Line r -- extend the region to full lines.
   modifyRegionB (mapLines (R.cons ' ')) r'


main :: IO ()
main = yi $ defaultEmacsConfig
  { defaultKm =
      Emacs.mkKeymap $ override Emacs.defKeymap $ \parent _self ->
        parent {
           _eKeymap = (_eKeymap parent) ||> (metaCh '>' ?>>! increaseIndent)
        }
      -- bind M-> to increaseIndent and mix with default Emacs keymap.
  }

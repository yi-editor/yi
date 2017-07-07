import           Lens.Micro.Platform ((.=))
import           Data.Prototype (override)
import           Yi.Boot (configMain)
import           Yi.Config.Default.Emacs
import           Yi.Config.Default.MiscModes
import           Yi.Config.Default.Vty
import           Yi.Config.Simple
import           Yi.Keymap.Emacs as Emacs
import qualified Yi.Rope as R
import           Yi.String           (mapLines)

main :: IO ()
main = configMain defaultConfig $ do
         configureVty
         myEmacsConfig
         configureMiscModes

myEmacsConfig :: ConfigM ()
myEmacsConfig = do
    configureEmacs
    defaultKmA .= myKeymapSet

myKeymapSet :: KeymapSet
myKeymapSet =
      Emacs.mkKeymapSet $ Emacs.defKeymap `override` \parent _self ->
        parent {
           -- bind M-> to increaseIndent and mix with default Emacs keymap.
           _eKeymap = (_eKeymap parent) ||> (metaCh '>' ?>>! increaseIndent)
        }

increaseIndent :: BufferM ()
increaseIndent = do
   r <- getSelectRegionB
   r' <- unitWiseRegion Line r -- extend the region to full lines.
   modifyRegionB (mapLines (R.cons ' ')) r'

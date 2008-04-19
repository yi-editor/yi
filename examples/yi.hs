import Yi
import Yi.Keymap.Emacs (keymap)
import qualified Yi.Mode.Shim as Shim
import Yi.UI.Common (UIConfig(..))
import Yi.Mode.Haskell
import Data.List (isSuffixOf)
import Yi.Prelude
import Prelude ()


myModetable :: ReaderT String Maybe AnyMode
myModetable = ReaderT $ \fname -> case () of 
                        _ | ".hs" `isSuffixOf` fname -> Just $ AnyMode bestHaskellMode
                        _ ->  Nothing
    where bestHaskellMode = cleverHaskellMode 
                            {
                             modeKeymap = modeKeymap Shim.mode
                            }


main :: IO ()
main = yi $ defaultConfig {
                           modeTable = myModetable <|> modeTable defaultConfig,
                           configUI = (configUI defaultConfig) { configFontSize = Just 10 },
                           defaultKm = keymap
                          }

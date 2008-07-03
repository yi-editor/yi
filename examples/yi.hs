import Yi
import Yi.Keymap.Emacs (keymap)
-- import  Yi.Keymap.Cua (keymap)
import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell
import Data.List (isSuffixOf)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys

myModetable :: ReaderT String Maybe AnyMode
myModetable = ReaderT $ \fname -> case () of 
                        _ | ".hs" `isSuffixOf` fname -> Just $ AnyMode bestHaskellMode
                        _ ->  Nothing
    where bestHaskellMode = cleverHaskellMode 
                            {
                             modeKeymap = ((ctrl (char 'c') ?>> ctrl(char 'c') ?>>! haskellToggleCommentSelectionB)
                                           <||) . 
                                          modeKeymap Shim.mode
                            }


main :: IO ()
main = yi $ defaultConfig {
                           configKillringAccumulate = True,
                           modeTable = myModetable <|> modeTable defaultConfig,
                           configUI = (configUI defaultConfig) { configFontSize = Just 10 },
                           defaultKm = keymap
                          }

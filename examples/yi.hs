import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim
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
                             -- example of Mode-local rebinding
                             modeKeymap = ((ctrl (char 'c') ?>> ctrl(char 'c') ?>>! haskellToggleCommentSelectionB)
                                           <||)  
                              -- uncomment this for Shim (dot is important!)
                              -- . modeKeymap Shim.mode
                            }

greek = [("alpha", "α"),
         ("beta", "β"),
         ("gamma", "γ"),
         ("delta", "δ")
        ]

extraInput :: Keymap
extraInput = choice [pString ('\\':i) >>! insertN o | (i,o) <- greek]

main :: IO ()
main = yi $ defaultConfig {
                           configKillringAccumulate = True,
                           modeTable = myModetable <|> modeTable defaultConfig,
                           configUI = (configUI defaultConfig) { configFontSize = Just 10 },
                           defaultKm = extraInput <|| keymap
                          }

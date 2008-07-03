import Yi
import Yi.Keymap.Vim (keymap)
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
                             modeKeymap = modeKeymap Shim.mode
                            }

Config {
        configUI = defUI@UIConfig { configStyle = defStyle }
       } = defaultConfig

{-
  For now we just make the selected style the same as the
  modeline_focused style... Just because i'm not good with
  styles yet - Jim
-}
defaultVimUiStyle :: Style.UIStyle
defaultVimUiStyle = defStyle { selected = Style.modeline_focused Style.uiStyle}



main :: IO ()
main = yi $ defaultConfig {
                           modeTable = myModetable <|> modeTable defaultConfig,
                           configUI = defConfigUI { configFontSize = Just 10,
                                                    configStyle = defaultVimUiStyle,
                                                    configWindowFill = '~'
                                                  },
                           defaultKm = keymap
                          }

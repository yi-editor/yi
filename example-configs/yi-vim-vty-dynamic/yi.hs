import Yi.Config.Simple
import Yi.Config.Default
import Yi.Config.Default.Vty
import Yi.Config.Default.Vim
import Yi.Config.Default.HaskellMode
import Yi.Config.Default.JavaScriptMode
import Yi.Config.Default.MiscModes

main :: IO ()
main = configMain defaultConfig $ do
         configureVty
         configureVim
         configureHaskellMode
         configureJavaScriptMode
         configureMiscModes

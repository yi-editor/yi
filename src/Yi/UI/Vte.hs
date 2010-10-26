module Yi.UI.Vte (start) where

import Prelude ()
import Yi.Prelude

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import System.Environment.Executable
import System.Glib

import Yi.Config
import Yi.Style
import qualified Yi.UI.Common as Common

start :: UIBoot
start cfg ch outCh editor =
    catchGError (initUI cfg ch outCh editor) (\(GError _dom _code msg) -> fail msg)

initUI :: UIBoot
initUI cfg ch outCh editor = do
    discard unsafeInitGUIForThreadedRTS
    setApplicationName "Yi"

    -- Setup window
    win <- windowNew
    discard $ win `onDestroy` mainQuit

    -- Setup vte
    exe  <- getExecutablePath
    term <- terminalNew
    Graphics.UI.Gtk.on term childExited $ end False

    -- Set default colors
    terminalSetColors term
        (getBaseAttrColor foreground black cfg)
        (getBaseAttrColor background brightwhite cfg)
        (Color 0 0 0)
        0

    -- Start running Yi
    discard $ terminalForkCommand term
        (Just exe) (Just [exe, "-fvty"]) Nothing Nothing False False False

    discard $ set win [ containerChild := term ]
    widgetShowAll win

    return $ Common.dummyUI
      { Common.main = main
      , Common.end  = end
      }

main :: IO ()
main = mainGUI

end :: Bool -> IO ()
end = const mainQuit

getBaseAttrColor p d cfg = mkCol $
    case p $ baseAttributes $ configStyle $ configUI cfg of
      Default -> d
      c -> c

mkCol :: Yi.Style.Color -> Graphics.UI.Gtk.Color
mkCol Default     = Color 0 0 0
mkCol (RGB x y z) = Color (fromIntegral x * 256)
                          (fromIntegral y * 256)
                          (fromIntegral z * 256)

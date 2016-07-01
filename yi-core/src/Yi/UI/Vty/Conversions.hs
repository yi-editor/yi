module Yi.UI.Vty.Conversions
    ( colorToAttr
    , fromVtyEvent
    , fromVtyKey
    , fromVtyMod
    ) where

import           Data.List    (nub, sort)
import qualified Graphics.Vty as Vty (Attr, Color, Event (EvKey), Key (KBS, KBackTab, KBegin, KCenter, KChar, KDel, KDown, KEnd, KEnter, KEsc, KFun, KHome, KIns, KLeft, KMenu, KPageDown, KPageUp, KPause, KPrtScr, KRight, KUp),
                                      Modifier (..), black, blue, brightBlack,
                                      brightBlue, brightCyan, brightGreen,
                                      brightMagenta, brightRed, brightWhite,
                                      brightYellow, cyan, green, magenta, red,
                                      rgbColor, white, yellow)
import qualified Yi.Event     (Event (..), Key (..), Modifier (MCtrl, MMeta, MShift))
import qualified Yi.Style     (Color (..))

fromVtyEvent :: Vty.Event -> Yi.Event.Event
fromVtyEvent (Vty.EvKey Vty.KBackTab mods) =
    Yi.Event.Event Yi.Event.KTab (sort $ nub $ Yi.Event.MShift : map fromVtyMod mods)
fromVtyEvent (Vty.EvKey k mods) =
    Yi.Event.Event (fromVtyKey k) (sort $ map fromVtyMod mods)
fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."

fromVtyKey :: Vty.Key -> Yi.Event.Key
fromVtyKey (Vty.KEsc      ) = Yi.Event.KEsc
fromVtyKey (Vty.KFun x    ) = Yi.Event.KFun x
fromVtyKey (Vty.KPrtScr   ) = Yi.Event.KPrtScr
fromVtyKey (Vty.KPause    ) = Yi.Event.KPause
fromVtyKey (Vty.KChar '\t') = Yi.Event.KTab
fromVtyKey (Vty.KChar c   ) = Yi.Event.KASCII c
fromVtyKey (Vty.KBS       ) = Yi.Event.KBS
fromVtyKey (Vty.KIns      ) = Yi.Event.KIns
fromVtyKey (Vty.KHome     ) = Yi.Event.KHome
fromVtyKey (Vty.KPageUp   ) = Yi.Event.KPageUp
fromVtyKey (Vty.KDel      ) = Yi.Event.KDel
fromVtyKey (Vty.KEnd      ) = Yi.Event.KEnd
fromVtyKey (Vty.KPageDown ) = Yi.Event.KPageDown
fromVtyKey (Vty.KCenter   ) = Yi.Event.KNP5
fromVtyKey (Vty.KUp       ) = Yi.Event.KUp
fromVtyKey (Vty.KMenu     ) = Yi.Event.KMenu
fromVtyKey (Vty.KLeft     ) = Yi.Event.KLeft
fromVtyKey (Vty.KDown     ) = Yi.Event.KDown
fromVtyKey (Vty.KRight    ) = Yi.Event.KRight
fromVtyKey (Vty.KEnter    ) = Yi.Event.KEnter
fromVtyKey (Vty.KBackTab  ) = error "This should be handled in fromVtyEvent"
fromVtyKey (Vty.KBegin    ) = error "Yi.UI.Vty.fromVtyKey: can't handle KBegin"
fromVtyKey _ = error "Unhandled key in fromVtyKey"

fromVtyMod :: Vty.Modifier -> Yi.Event.Modifier
fromVtyMod Vty.MShift = Yi.Event.MShift
fromVtyMod Vty.MCtrl  = Yi.Event.MCtrl
fromVtyMod Vty.MMeta  = Yi.Event.MMeta
fromVtyMod Vty.MAlt   = Yi.Event.MMeta


-- | Convert a Yi Attr into a Vty attribute change.
colorToAttr :: (Vty.Color -> Vty.Attr -> Vty.Attr)
    -> Yi.Style.Color -> Vty.Attr -> Vty.Attr
colorToAttr set c =
  case c of
    Yi.Style.RGB 0 0 0         -> set Vty.black
    Yi.Style.RGB 128 128 128   -> set Vty.brightBlack
    Yi.Style.RGB 139 0 0       -> set Vty.red
    Yi.Style.RGB 255 0 0       -> set Vty.brightRed
    Yi.Style.RGB 0 100 0       -> set Vty.green
    Yi.Style.RGB 0 128 0       -> set Vty.brightGreen
    Yi.Style.RGB 165 42 42     -> set Vty.yellow
    Yi.Style.RGB 255 255 0     -> set Vty.brightYellow
    Yi.Style.RGB 0 0 139       -> set Vty.blue
    Yi.Style.RGB 0 0 255       -> set Vty.brightBlue
    Yi.Style.RGB 128 0 128     -> set Vty.magenta
    Yi.Style.RGB 255 0 255     -> set Vty.brightMagenta
    Yi.Style.RGB 0 139 139     -> set Vty.cyan
    Yi.Style.RGB 0 255 255     -> set Vty.brightCyan
    Yi.Style.RGB 165 165 165   -> set Vty.white
    Yi.Style.RGB 255 255 255   -> set Vty.brightWhite
    Yi.Style.Default           -> id
    Yi.Style.RGB r g b         -> set (Vty.rgbColor r g b)

module Yi.Vty
    (
     styleToAttr,
     defaultSty,

     module Vty
    ) where

import Yi.Style as Style

import Graphics.Vty as Vty

------------------------------------------------------------------------

--
-- Combine attribute with another attribute
--
boldA, reverseA, nullA :: Vty.Attr -> Vty.Attr
boldA       = setBold
reverseA    = setRV
nullA       = id

------------------------------------------------------------------------

newtype CColor = CColor (Vty.Attr -> Vty.Attr, Vty.Color)
--
-- | Map Style rgb rgb colours to ncurses pairs
-- TODO a generic way to turn an rgb into the nearest curses color
--
style2curses :: Style -> (CColor, CColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)
{-# INLINE style2curses #-}

fgCursCol :: Style.Color -> CColor
fgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA,    Vty.black)
    RGB 128 128 128   -> CColor (boldA,    Vty.black)
    RGB 139 0 0       -> CColor (nullA,    Vty.red)
    RGB 255 0 0       -> CColor (boldA,    Vty.red)
    RGB 0 100 0       -> CColor (nullA,    Vty.green)
    RGB 0 128 0       -> CColor (boldA,    Vty.green)
    RGB 165 42 42     -> CColor (nullA,    Vty.yellow)
    RGB 255 255 0     -> CColor (boldA,    Vty.yellow)
    RGB 0 0 139       -> CColor (nullA,    Vty.blue)
    RGB 0 0 255       -> CColor (boldA,    Vty.blue)
    RGB 128 0 128     -> CColor (nullA,    Vty.magenta)
    RGB 255 0 255     -> CColor (boldA,    Vty.magenta)
    RGB 0 139 139     -> CColor (nullA,    Vty.cyan)
    RGB 0 255 255     -> CColor (boldA,    Vty.cyan)
    RGB 165 165 165   -> CColor (nullA,    Vty.white)
    RGB 255 255 255   -> CColor (boldA,    Vty.white)
    Default           -> CColor (nullA,    Vty.def)
    Reverse           -> CColor (reverseA, Vty.def)
    _                 -> CColor (nullA,    Vty.black) -- NB

bgCursCol :: Style.Color -> CColor
bgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA,    Vty.black)
    RGB 128 128 128   -> CColor (nullA,    Vty.black)
    RGB 139 0 0       -> CColor (nullA,    Vty.red)
    RGB 255 0 0       -> CColor (nullA,    Vty.red)
    RGB 0 100 0       -> CColor (nullA,    Vty.green)
    RGB 0 128 0       -> CColor (nullA,    Vty.green)
    RGB 165 42 42     -> CColor (nullA,    Vty.yellow)
    RGB 255 255 0     -> CColor (nullA,    Vty.yellow)
    RGB 0 0 139       -> CColor (nullA,    Vty.blue)
    RGB 0 0 255       -> CColor (nullA,    Vty.blue)
    RGB 128 0 128     -> CColor (nullA,    Vty.magenta)
    RGB 255 0 255     -> CColor (nullA,    Vty.magenta)
    RGB 0 139 139     -> CColor (nullA,    Vty.cyan)
    RGB 0 255 255     -> CColor (nullA,    Vty.cyan)
    RGB 165 165 165   -> CColor (nullA,    Vty.white)
    RGB 255 255 255   -> CColor (nullA,    Vty.white)
    Default           -> CColor (nullA,    Vty.def)
    Reverse           -> CColor (reverseA, Vty.def)
    _                 -> CColor (nullA,    Vty.white)    -- NB

defaultSty :: Style
defaultSty = Style Default Default

styleToAttr :: Style -> Vty.Attr
styleToAttr = ccolorToAttr . style2curses
    where ccolorToAttr ((CColor (fmod, fcolor)), (CColor (bmod, bcol))) = 
              fmod . bmod . setFG fcolor . setBG bcol $ attr

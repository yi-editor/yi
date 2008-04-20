--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

--
-- | Colors and friends.
--

module Yi.Style where

import Data.Word                (Word8)
import Data.Char (chr, ord)

-- | The UI type
data UIStyle =
    UIStyle {
         window           :: !Style    -- ^ window fg and bg (ignore for now)
       , modeline         :: !Style    -- ^ out of focus modeline colours
       , modeline_focused :: !Style    -- ^ in focus modeline
       , selected         :: !Style    -- ^ the selected portion
       , eof              :: !Style    -- ^ empty file marker colours
     } 
    deriving (Eq, Show, Ord)

data Color
    = RGB {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | Default
    | Reverse
    deriving (Eq,Ord,Show)


-- | Convert a color to its text specification, as to be accepted by XParseColor
colorToText :: Color -> String
colorToText Default = "default"
colorToText Reverse = "reverse"
colorToText (RGB r g b) = ('#':) . showsHex r . showsHex g . showsHex b $ []
    where showsHex x s = showHex1 (x `div` 16) : showHex1 (x `mod` 16) : s
          showHex1 x | x < 10 = chr (ord '0' + fromIntegral x)
                     | otherwise = chr (ord 'A' + fromIntegral x - 10)



-- | Default settings
uiStyle :: UIStyle
uiStyle = UIStyle {
         window             = Style defaultfg    defaultbg
        ,modeline           = Style black        darkcyan
        ,modeline_focused   = Style brightwhite  darkcyan
        ,selected           = Style reversefg    reversebg
        ,eof                = Style blue         defaultbg
     }

defaultStyle :: Style
defaultStyle = Style defaultfg defaultbg

commentStyle :: Style
commentStyle = lineCommentStyle

lineCommentStyle :: Style
lineCommentStyle = purpleA

keywordStyle :: Style
keywordStyle = darkblueA

operatorStyle :: Style
operatorStyle = brownA

upperIdStyle :: Style
upperIdStyle = darkgreenA

stringStyle :: Style
stringStyle = darkcyanA

numberStyle :: Style
numberStyle = darkredA

errorStyle :: Style -> Style
errorStyle (Style _fg bg) = Style red bg

hintStyle :: Style -> Style
hintStyle (Style fg _bg) = Style fg cyan

blackA , greyA, darkredA, redA, darkgreenA, greenA, brownA :: Style
yellowA, darkblueA, blueA, purpleA, magentaA, darkcyanA    :: Style
cyanA, whiteA, brightwhiteA                                :: Style
blackA       = Style black defaultbg
greyA        = Style grey defaultbg
darkredA     = Style darkred defaultbg
redA         = Style red defaultbg
darkgreenA   = Style darkgreen defaultbg
greenA       = Style green defaultbg
brownA       = Style brown defaultbg
yellowA      = Style yellow defaultbg
darkblueA    = Style darkblue defaultbg
blueA        = Style blue defaultbg
purpleA      = Style purple defaultbg
magentaA     = Style magenta defaultbg
darkcyanA    = Style darkcyan defaultbg
cyanA        = Style cyan defaultbg
whiteA       = Style white defaultbg
brightwhiteA = Style brightwhite defaultbg

------------------------------------------------------------------------

-- | Foreground and background color pairs
data Style = Style {-# UNPACK #-} !Color !Color deriving (Eq,Ord,Show)

------------------------------------------------------------------------

-- | Some simple colours (derivied from proxima/src/common/CommonTypes.hs)

black, grey, darkred, red, darkgreen, green, brown, yellow          :: Color
darkblue, blue, purple, magenta, darkcyan, cyan, white, brightwhite :: Color
black       = RGB 0 0 0
grey        = RGB 128 128 128
darkred     = RGB 139 0 0
red         = RGB 255 0 0
darkgreen   = RGB 0 100 0
green       = RGB 0 128 0
brown       = RGB 165 42 42
yellow      = RGB 255 255 0
darkblue    = RGB 0 0 139
blue        = RGB 0 0 255
purple      = RGB 128 0 128
magenta     = RGB 255 0 255
darkcyan    = RGB 0 139 139 
cyan        = RGB 0 255 255
white       = RGB 165 165 165
brightwhite = RGB 255 255 255

defaultfg, defaultbg, reversefg, reversebg :: Color
defaultfg   = Default
defaultbg   = Default
reversefg   = Reverse
reversebg   = Reverse


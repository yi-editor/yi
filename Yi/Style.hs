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

--
-- | The UI type
--
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
    = RGB {-# UNPACK #-} !Word8 !Word8 !Word8
    | Default
    | Reverse
    deriving (Eq,Ord,Show)

-- | Convert a color to its text specification, as to be accepted by XParseColor

showHex1 :: Word8 -> Char
showHex1 x | x < 10 = chr (ord '0' + fromIntegral x)
           | otherwise = chr (ord 'A' + fromIntegral x - 10)

showsHex :: Word8 -> (String -> String)
showsHex x s =
    showHex1 (x `div` 16) : showHex1 (x `mod` 16) : s

colorToText :: Color -> String
colorToText Default = "black"
colorToText Reverse = "white"
colorToText (RGB r g b) = ('#':) . showsHex r . showsHex g . showsHex b $ []


--
-- | Default settings
--
{-
  Notice that the selected field is initially set to the default, which
  essentially means that selected text will not be highlighted. The reason
  for this is that if the mode does not remember to UnSet the mark after,
  for example, cutting, then there will always be a highlighted region, that
  is essentially the default for now which has worked up until now because the
  selected text wasn't highlighted, now that it is, we need the modes to unset
  the mark when nothing should be hightlighted.
-}
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


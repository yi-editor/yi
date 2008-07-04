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

data Attr = Foreground !Color 
          | Background !Color
    deriving (Eq, Show, Ord)

type Style = [Attr]

-- | Convert a color to its text specification, as to be accepted by XParseColor
colorToText :: Color -> String
colorToText Default = "default"
colorToText Reverse = "reverse"
colorToText (RGB r g b) = ('#':) . showsHex r . showsHex g . showsHex b $ []
    where showsHex x s = showHex1 (x `div` 16) : showHex1 (x `mod` 16) : s
          showHex1 x | x < 10 = chr (ord '0' + fromIntegral x)
                     | otherwise = chr (ord 'A' + fromIntegral x - 10)


defaultStyle :: Style
defaultStyle = []

reverseStyle :: Style
reverseStyle = [Foreground Reverse, Background Reverse]


commentStyle :: Style
commentStyle = lineCommentStyle

lineCommentStyle :: Style
lineCommentStyle = [Foreground purple]

keywordStyle :: Style
keywordStyle = [Foreground darkblue]

operatorStyle :: Style
operatorStyle = [Foreground brown]

upperIdStyle :: Style
upperIdStyle = [Foreground darkgreen]

stringStyle :: Style
stringStyle = [Foreground darkcyan]

numberStyle :: Style
numberStyle = [Foreground darkred]

errorStyle :: Style
errorStyle = [Background red]

hintStyle :: Style
hintStyle = [Background white]

strongHintStyle :: Style
strongHintStyle = [Background cyan]


------------------------------------------------------------------------

-- | Some simple colours

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



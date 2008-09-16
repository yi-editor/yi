-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | Colors and friends.
module Yi.Style where

import Data.Word                (Word8)
import Data.Char (chr, ord)
import Data.Monoid
import Yi.Prelude
import Prelude ()

-- | The UI type
data UIStyle =
    UIStyle {
         window            :: Style -- ^ window fg and bg
       , modeline          :: Style -- ^ out of focus modeline colours
       , modelineFocused   :: Style -- ^ in focus modeline
       , selected          :: Style -- ^ the selected portion
       , eof               :: Style -- ^ empty file marker colours
       , preprocessorStyle :: Style -- ^ preprocessor directive (often in Haskell or C)
       , commentStyle      :: Style
       , blockCommentStyle :: Style
       , keywordStyle      :: Style

       , operatorStyle     :: Style

       , variableStyle     :: Style -- ^ any standard variable (identifier)
       , typeStyle         :: Style -- ^ class (in an OO language) or a Haskell type

       , stringStyle       :: Style
       , longStringStyle   :: Style
       , numberStyle       :: Style

       , errorStyle        :: Style
       , hintStyle         :: Style
       , strongHintStyle   :: Style
     } 
--    deriving (Eq, Show)

type StyleName = UIStyle -> Style

-- | A style transforms some of the visual text attributes.
type Style = Endo Attributes

-- | Visual text attributes to be applied during layout.
data Attributes = Attributes
  { foreground :: !Color
  , background :: !Color
  } deriving (Eq, Ord, Show)

withFg, withBg :: Color -> Style
-- | A style that sets the foreground.
withFg c = Endo $ \s -> s { foreground = c }
-- | A style that sets the background.
withBg c = Endo $ \s -> s { background = c }

-- | The identity transform.
defaultStyle :: StyleName
defaultStyle = mempty

-- | Apply the styles to the default attributes.
appStyle :: Style -> Attributes
appStyle = flip appEndo defaultAttributes

defaultAttributes :: Attributes
defaultAttributes = Attributes
  { foreground = Default
  , background = Default
  }


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

------------------------------------------------------------------------

-- Some simple colours

black, grey, lightGrey, darkred, red, darkgreen, green, brown, yellow :: Color
darkblue, blue, purple, magenta, darkcyan, cyan, white, brightwhite   :: Color
black       = RGB 0 0 0
grey        = RGB 128 128 128
lightGrey   = RGB 100 100 100
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


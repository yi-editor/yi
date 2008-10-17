-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | Colors and friends.
module Yi.Style where

import Data.Word                (Word8)
import Data.Char (chr, ord)
import Data.Monoid
import Yi.Prelude
import Prelude ()

-- | Visual text attributes to be applied during layout.
data Attributes = Attributes
  { foreground :: !Color
  , background :: !Color
  } deriving (Eq, Ord, Show)

-- | The style is used to transform attributes by modifying
--   one or more of the visual text attributes.
type Style = Endo Attributes

-- | The UI type
data UIStyle = UIStyle
  { modelineAttributes :: Attributes -- ^ ground attributes for the modeline
  , modelineFocusStyle :: Style      -- ^ transformation of modeline in focus

  , baseAttributes     :: Attributes -- ^ ground attributes for the main text views

  -- General styles applied to the ground attributes above
  , selectedStyle      :: Style      -- ^ the selected portion
  , eofStyle           :: Style      -- ^ empty file marker colours

  , errorStyle         :: Style      -- ^ indicates errors in text
  , hintStyle          :: Style      -- ^ search matches/paren matches/other hints
  , strongHintStyle    :: Style      -- ^ current search match

  -- Syntax highlighting styles
  , commentStyle       :: Style      -- ^ all comments
  , blockCommentStyle  :: Style      -- ^ additional only for block comments
  , keywordStyle       :: Style      -- ^ applied to language keywords
  , numberStyle        :: Style      -- ^ numbers
  , preprocessorStyle  :: Style      -- ^ preprocessor directive (often in Haskell or C)
  , stringStyle        :: Style      -- ^ constant strings
  , longStringStyle    :: Style      -- ^ additional style for long strings
  , typeStyle          :: Style      -- ^ type name (such as class in an OO language)
  , variableStyle      :: Style      -- ^ any standard variable (identifier)
  , operatorStyle      :: Style      -- ^ infix operators
  }

type StyleName = UIStyle -> Style

withFg, withBg :: Color -> Style
-- | A style that sets the foreground.
withFg c = Endo $ \s -> s { foreground = c }
-- | A style that sets the background.
withBg c = Endo $ \s -> s { background = c }

-- | The identity transform.
defaultStyle :: StyleName
defaultStyle = mempty

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


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
  , reverseAttr :: !Bool
    -- ^ The text should be show as "active" or "selected".
    -- This can be implemented by reverse video on the terminal.
  , bold :: !Bool
  , italic :: !Bool
  , underline :: !Bool
  } deriving (Eq, Ord, Show)

emptyAttributes :: Attributes
emptyAttributes = Attributes { foreground = Default, background = Default, reverseAttr = False, bold = False, italic = False, underline = False }

-- | The style is used to transform attributes by modifying
--   one or more of the visual text attributes.
type Style = Endo Attributes

-- | The UI type
data UIStyle = UIStyle
  { modelineAttributes :: Attributes -- ^ ground attributes for the modeline
  , modelineFocusStyle :: Style      -- ^ transformation of modeline in focus
  
  , tabBarAttributes   :: Attributes -- ^ ground attributes for the tabbar
  , tabInFocusStyle    :: Style      -- ^ a tab that currently holds the focus
  , tabNotFocusedStyle :: Style      -- ^ a tab that does not have the current focus

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
  , dataConstructorStyle
                       :: Style      -- ^ data constructor
  , importStyle        :: Style      -- ^ style of import names
  , builtinStyle       :: Style      -- ^ builtin things, e.g. Array in JavaScript
  , regexStyle         :: Style      -- ^ regular expressions
  , variableStyle      :: Style      -- ^ any standard variable (identifier)
  , operatorStyle      :: Style      -- ^ infix operators

  , quoteStyle         :: Style      -- ^ Style of a quotation (e.g. in template haskell)    

  , makeFileAction     :: Style      -- ^ stuff that's passed to the shell in a Makefile
  , makeFileRuleHead   :: Style      -- ^ makefile rule headers
  }

-- | A StyleName determines what style to use, taking into account the
-- set of rendering preferences given by a 'UIStyle'.  Typically, style
-- names will be 'Style'-valued field names of 'UIStyle'.
type StyleName = UIStyle -> Style

withFg, withBg :: Color -> Style
-- | A style that sets the foreground.
withFg c = Endo $ \s -> s { foreground = c }
-- | A style that sets the background.
withBg c = Endo $ \s -> s { background = c }

withBd, withItlc, withUnderline :: Bool -> Style
-- | A style that sets the font to bold
withBd c = Endo $ \s -> s { bold = c }
-- | A style that sets the style to italics
withItlc c = Endo $ \s -> s { italic = c }
-- | A style that sets the style to underlined
withUnderline c = Endo $ \s -> s { underline = c }
-- | A style that sets the style to underlined
withReverse c = Endo $ \s -> s { reverseAttr = c }

-- | The identity transform.
defaultStyle :: StyleName
defaultStyle = mempty

data Color
    = RGB {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | Default 
    -- ^ The system-default color of the engine used.
    -- e.g. in Gtk this should pick whatever the user has chosen as default color 
    -- (background or forground depending on usage) for the text.
    deriving (Eq,Ord,Show)

-- | Convert a color to its text specification, as to be accepted by XParseColor
colorToText :: Color -> String
colorToText Default = "default"
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

import Yi

-- Preamble
import Yi.Prelude
import Prelude (take, length, repeat)
import Yi.Keymap.Vim(mkKeymap,defKeymap,ModeMap(..))

import Data.Time.Clock(getCurrentTime)
import System.Locale(defaultTimeLocale)
import Data.Time.Format(formatTime)

import Yi.UI.Vty (start)
-- import Yi.UI.Cocoa (start)
import Yi.UI.Pango (start)

-- Used in Theme declaration
import Data.Monoid(mappend)

-- inserting current date and underline
currentDate :: IO String
currentDate =
    do tim <- Data.Time.Clock.getCurrentTime
       return $ formatTime locale  "%A %b %e %Y" tim
  where locale = System.Locale.defaultTimeLocale

makeUnderline :: String -> String
makeUnderline s = s ++ ('\n' : line) ++ "\n"
  where
    line = take (length s) (repeat '=')
      
currentDateAndUnderline :: IO String
currentDateAndUnderline =
    do d <- currentDate
       return $ makeUnderline d

insertCurrentDate :: YiM ()
insertCurrentDate = do d <- withUI (\_ -> currentDateAndUnderline)
                       withBuffer (insertN d)

extendedVimKeymap = defKeymap `override` \super self -> super
    {
        v_top_level = 
            (deprioritize >> v_top_level super)
            <|> (ctrlCh 'd' ?>>! insertCurrentDate),
            -- On 'o' in normal mode I always want to use the indent of the previous line.
            -- TODO: If the line where the newline is to be inserted is inside a
            -- block comment then the block comment should be "continued"
            -- TODO: Ends up I'm trying to replicate vim's "autoindent" feature. This 
            -- should be made a function in Yi.
        v_ins_char = 
            (deprioritize >> v_ins_char super) 
            -- On starting to write a block comment I want the close comment 
            -- text inserted automatically.
            <|> choice 
                [ pString open_tag >>! do
                    insertN $ open_tag ++ " \n" 
                    indentAsPreviousB
                    insertN $ " " ++ close_tag
                    lineUp
                 | (open_tag, close_tag) <- 
                    [ ("{-", "-}") -- Haskell block comments
                    , ("/*", "*/") -- C++ block comments
                    ]
                ]
    }

-- This is based on Vim's ':colorscheme murphy', but with gray strings, and more brown on operators.

myTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes   = emptyAttributes { foreground = black, background = darkcyan         }
  , tabBarAttributes     = emptyAttributes { foreground = white, background = black            }
  , baseAttributes       = emptyAttributes { foreground = green, background = black, bold=True }
  , commentStyle         = withFg darkred `mappend` withBd False `mappend` withItlc True
--  , selectedStyle        = withFg black   `mappend` withBg green `mappend` withReverse True
  , selectedStyle        = withReverse True
  , errorStyle           = withBg red     `mappend` withFg white
  , operatorStyle        = withFg brown   `mappend` withBd False
  , hintStyle            = withBg brown   `mappend` withFg black
  , importStyle          = withFg blue
  , dataConstructorStyle = withFg blue
  , typeStyle            = withFg blue
  , keywordStyle         = withFg yellow
  , builtinStyle         = withFg brown
  , strongHintStyle      = withBg brown   `mappend` withUnderline True
  , stringStyle          = withFg brown   `mappend` withBd False
  , preprocessorStyle    = withFg blue
--  , constantStyle      = withFg cyan
--  , specialStyle      = withFg yellow
  }

-- Softtabs of 2 characters for Berkeley coding style, if not editing makefile.
prefIndent :: Mode s -> Mode s
prefIndent m = if modeName m == "Makefile"
                 then m
                 else
      m {
        modeIndentSettings = IndentSettings
            {
                expandTabs = True,
                shiftWidth = 2,
                tabSize    = 2
            }
        }


defaultUIConfig = configUI defaultVimConfig

main :: IO ()
main = yi $ defaultVimConfig
  {
   -- Keymap Configuration
   defaultKm = mkKeymap extendedVimKeymap,
   modeTable = fmap (onMode $ prefIndent) (modeTable defaultVimConfig),

   -- Options:
   configUI = defaultUIConfig
     { 
       configTheme = myTheme,       
       configWindowFill = ' ' 
                          -- '~'    -- Typical for Vim
     }
  }


{-
  A yi configuration file demonstrating a keymap based on
  the default emacs keybindings with a few additional/overridden
  key sequences.
  It should be fairly easy to add your own from here.
-}

{- Standard Library Modules Imported -}
import Data.Char
  ( isDigit
  , isSpace
  )

import qualified Data.Map as M
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Yi
import Yi.Yi
import Yi.Keymap.Emacs


-- I wish to start the vty by default.
import qualified Yi.UI.Vty as Vty

import Yi.Keymap.Emacs.Utils
  ( insertTemplate
  , queryReplaceE
  , changeBufferNameE
  , withMinibuffer
  )

import Yi.Keymap.Emacs.Keys
  ( KList )
import Yi.Keymap.Emacs.KillRing
  ( killRingSaveE )

import Yi.TextCompletion
  ( completeWordB )

import Yi.Indent
  ( indentAsPreviousB )

import Yi.Buffer.HighLevel
{- End of Module Imports -}

-- This is the main function, which binds
-- user configuration.

main :: IO ()
main = yi $ defaultConfig { startFrontEnd    = Vty.start
                          , defaultKm        = myKeyMap
                          , publishedActions = myAndDefaultActions
                          }


-- So your keymap will be the emacs keybindings
-- 'keymap' with your additional/overriding key
-- binding definitions.
myKeyMap :: KeymapM ()
myKeyMap = (makeKeymap myKeys) <|| keymap

-- The list of keys that you wish to override/add to
-- the default emacs keybindings.
-- Add/remove from here as you desire.
myKeys :: KList
myKeys = 
  [ -- I like this for completing a word
    -- note that this is overriding the emacs default
    -- to move back a character.
    ("C-b",      write completeWordB)

    -- For indenting the same as the previous line.
    -- I'd like to bind this to C-i but that messes up Tab.
  , ("C-o",      write indentAsPreviousB)

    -- For changing the buffer name. This can be useful
    -- if you have two files with the same name in different
    -- directories.
  , ("C-x C-t",  write changeBufferNameE)

    -- Inserting a template, generally for a new file.
  , ("C-x C-y",  write insertTemplate)

    -- I like C-x l to be gotoLn.
    -- Additionally I prefer the 'gotoLineE' function which
    -- allows you to additionally append a column number onto
    -- the line number.
  , ("C-x l",    write gotoLineE )

     -- Now comes some rebinding because the 'emacs' way clashes
     -- with xmonad, admittedly some are also just a personal preference.
     -- M-% clashes with xmonad
  , ("C-x t",    write queryReplaceE)
   
    -- M-w clashes with xmonad
  , ("C-x w",    write killRingSaveE)


    -- Now come some commands for manipulating the selected region
    -- I personally attach these all to C-l followed by another key.
    -- So bascially C-l means I want to do something with the region
    -- and the next key press is what I wish to do with it.

    -- First up comment the selection using haskell line comments.
  , ("C-x h",    write haskellCommentSelectionB)
  , ("C-x i",    write funnyModifySelectionB) 
    -- , ("C-x i",   write indentAsFirstSelectionB)
  ]

myAndDefaultActions :: M.Map String Action
myAndDefaultActions = M.union myActions defaultPublishedActions

myActions :: M.Map String Action
myActions =
  M.fromList [ ( "haskell-comment-region"
               , makeAction haskellCommentSelectionB
               )
             , ( "haskell-uncomment-region"
               , makeAction haskellUnCommentSelectionB
               )
             , ( "latex-comment-region"
               , makeAction latexCommentSelectionB
               )
             , ( "latex-uncomment-region"
               , makeAction latexUnCommentSelectionB 
               )
             , ( "increase-indent-region"
               , makeAction $ increaseIndentSelectionB 2
               )
             , ( "decrease-indent-region"
               , makeAction $ decreaseIndentSelectionB 2
               )
             ]

{-
  Helper functions
-}
funnyModifySelectionB :: BufferM ()
funnyModifySelectionB = modifySelectionB (const "hello")

-- | Goto a line specified in the mini buffer.
gotoLineE :: YiM ()
gotoLineE =
  withMinibuffer "Go to line:" return gotoAction
  where
  gotoAction :: String -> YiM ()
  gotoAction s =
    case parseLineAndChar s of
      Nothing     -> msgEditor "line and column number parse error"
      -- considering putting "gotoLineAndCol :: Int -> Int -> BufferM ()
      -- into Buffer.hs
      Just (l, c) -> withBuffer $ do gotoLn l
                                     rightN c

  -- This is actually relatively forgiving, for example "10.23xyh" will still
  -- take you to line number 10 column number 23
  -- in fact you can have any non digit character as the separator eg
  -- "10:24" or "10 23"
  -- In fact it need not be one character that is the separator, for example
  -- you can have: "3 my giddy aunt 43" and this will take you to line 3
  -- column 43.
  parseLineAndChar :: String -> Maybe (Int, Int)
  parseLineAndChar s
    | null lineString         = Nothing
    | null colString          = Just (read lineString, 0)
    | otherwise               = Just (read lineString, read colString)
    where
    (lineString, rest) = break (not . isDigit) $ dropWhile isSpace s
    colString          = takeWhile isDigit $ dropWhile (not . isDigit) rest

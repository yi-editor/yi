{-
  A yi configuration file demonstrating a keymap based on
  the default emacs keybindings with a few additional/overridden
  key sequences.
  It should be fairly easy to add your own from here.
-}

module YiConfig 
  ( yiMain ) 
where

{- Standard Library Modules Imported -}
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Yi.Core
  ( changeKeymap
  , msgEditor
  )
import Yi.Interact
  ( (<||) )
import Yi.Keymap
  ( YiM
  , KeymapM
  , write
  )

import Yi.Keymap.Emacs
  ( keymap ) -- The default emacs keybindings
import Yi.Keymap.Emacs.Keys
  ( KList
  , makeKeymap
  )

import Yi.Keymap.Emacs.Utils
  ( insertTemplate
  , queryReplaceE
  , changeBufferNameE
  , gotoLineE
  )

import Yi.Keymap.Emacs.KillRing
  ( killRingSaveE )

import Yi.TextCompletion
  ( completeWordB )

{- End of Module Imports -}

-- This is the main function which is called to complete
-- user configuration, it simply resets the key map and
-- and prints a message to say that it was successful.
yiMain :: YiM ()
yiMain = 
  do changeKeymap myKeyMap
     msgEditor "User configuration successful."

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

  ]

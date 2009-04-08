module Yi.Core
  ( module Yi.Dynamic
    -- * Keymap
  , module Yi.Keymap

  , module Yi.Prelude
  , module Yi.Editor
  , module Yi.Buffer
  , module Yi.Keymap.Keys

  -- * Construction and destruction
  , startEditor
  , quitEditor          -- :: YiM ()

  -- * User interaction
  , refreshEditor       -- :: YiM ()
  , suspendEditor       -- :: YiM ()
  , userForceRefresh

  -- * Global editor actions
  , msgEditor           -- :: String -> YiM ()
  , errorEditor         -- :: String -> YiM ()
  , closeWindow         -- :: YiM ()

  -- * Interacting with external commands
  , runProcessWithInput          -- :: String -> String -> YiM String
  , startSubprocess                 -- :: FilePath -> [String] -> YiM ()
  , sendToProcess

  -- * Misc
  , runAction
  , withSyntax

  )
where

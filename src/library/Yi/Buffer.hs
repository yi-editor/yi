
-- Copyright (C) 2008 JP Bernardy

-- | The 'Buffer' module defines monadic editing operations over one-dimensional
-- buffers, maintaining a current /point/.
--
-- This module acts as a Facade for the Buffer.* modules.

module Yi.Buffer
  ( module Yi.Buffer.Basic
  , module Yi.Buffer.HighLevel
  , module Yi.Buffer.Indent
  , module Yi.Buffer.Misc
  , module Yi.Buffer.Normal
  , module Yi.Buffer.Region
  , module Yi.Buffer.Undo
  -- Implementation re-exports (move out of implementation?)
  , UIUpdate (..)
  , Update (..)
  , updateIsDelete
  )
where

import Yi.Buffer.Basic
import Yi.Buffer.HighLevel
import Yi.Buffer.Indent
import Yi.Buffer.Misc
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Buffer.Undo

import Yi.Buffer.Implementation

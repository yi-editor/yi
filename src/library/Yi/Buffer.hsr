{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Buffer
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Buffer' module defines monadic editing operations over
-- one-dimensional buffers, maintaining a current /point/.
--
-- This module acts as a facade for the Buffer.* modules.

module Yi.Buffer
  ( module Yi.Buffer.Basic
  , module Yi.Buffer.HighLevel
  , module Yi.Buffer.Indent
  , module Yi.Buffer.Misc
  , module Yi.Buffer.Normal
  , module Yi.Buffer.Region
  , module Yi.Buffer.TextUnit
  , module Yi.Buffer.Undo
  -- Implementation re-exports (move out of implementation?)
  , UIUpdate (..)
  , Update (..)
  , updateIsDelete
  , markGravityAA
  , markPointAA
  )
where

import Yi.Buffer.Basic
import Yi.Buffer.HighLevel
import Yi.Buffer.Indent
import Yi.Buffer.Misc
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Buffer.TextUnit
import Yi.Buffer.Undo

import Yi.Buffer.Implementation

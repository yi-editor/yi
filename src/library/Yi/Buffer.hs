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
  ( module Export
  -- Implementation re-exports (move out of implementation?)
  , UIUpdate (..)
  , Update (..)
  , updateIsDelete
  , markGravityAA
  , markPointAA
  )
where

import Yi.Buffer.Basic     as Export
import Yi.Buffer.HighLevel as Export
import Yi.Buffer.Indent    as Export
import Yi.Buffer.Misc      as Export
import Yi.Buffer.Normal    as Export
import Yi.Buffer.Region    as Export
import Yi.Buffer.TextUnit  as Export
import Yi.Buffer.Undo      as Export

import Yi.Buffer.Implementation

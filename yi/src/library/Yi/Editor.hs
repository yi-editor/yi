{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Editor
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The top level editor state, and operations on it. This is inside an
-- internal module for easy re-export with Yi.Types bits.
module Yi.Editor ( Editor(..), EditorM(..), MonadEditor(..)
                 , runEditor
                 , module Yi.Editor.Internal
                 ) where

import Yi.Editor.Internal
import Yi.Types (Editor(..), EditorM(..), MonadEditor(..),
                 runEditor)

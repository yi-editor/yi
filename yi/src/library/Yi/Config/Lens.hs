{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Lens
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for types exported in Yi.Config. This module serves as a
-- convenience module, for easy re-exporting.

module Yi.Config.Lens where

import Yi.Types (Config(..), UIConfig(..))
import Yi.Utils (makeLensesWithSuffix)

makeLensesWithSuffix "A" ''Config
makeLensesWithSuffix "A" ''UIConfig

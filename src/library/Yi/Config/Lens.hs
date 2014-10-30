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

import Yi.Types (Config(..), UIConfig(..), YiConfigVariable)
import Yi.Utils (makeLensesWithSuffix)
import Control.Lens
import Data.DynamicState (_dyn)
import Data.Default

makeLensesWithSuffix "A" ''Config
makeLensesWithSuffix "A" ''UIConfig

configVariable :: YiConfigVariable a => Lens Config Config a a
configVariable = configVarsA . _dyn def

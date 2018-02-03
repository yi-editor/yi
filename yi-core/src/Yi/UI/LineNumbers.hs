{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.UI.LineNumbers
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Line numbers.

module Yi.UI.LineNumbers
  ( DisplayLineNumbers (..)
  , DisplayLineNumbersLocal (..)
  ) where

import           Data.Binary    (Binary (..))
import           Data.Default   (Default (..))
import           Data.Typeable  (Typeable)
import           GHC.Generics   (Generic)
import           Yi.Types       (YiVariable)

-- | A YiVariable which globally toggles line numbers for frontends
-- that support them.
newtype DisplayLineNumbers = DisplayLineNumbers { getDisplayLineNumbers :: Bool }
  deriving (Generic, Typeable)

instance Default DisplayLineNumbers where
  def = DisplayLineNumbers False

instance Binary DisplayLineNumbers

instance YiVariable DisplayLineNumbers

-- | Like 'DisplayLineNumbers' but buffer-local.
-- Nothing: use global settings
-- Just True: display line numbers only in this buffer
-- Just False: hide line numbers only in this buffer
newtype DisplayLineNumbersLocal = DisplayLineNumbersLocal { getDisplayLineNumbersLocal :: Maybe Bool }
  deriving (Generic, Typeable)

instance Default DisplayLineNumbersLocal where
  def = DisplayLineNumbersLocal Nothing

instance Binary DisplayLineNumbersLocal

instance YiVariable DisplayLineNumbersLocal

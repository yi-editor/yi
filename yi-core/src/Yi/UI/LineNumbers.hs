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
  ( getDisplayLineNumbersLocal
  , setDisplayLineNumbersLocal
  ) where

import           Data.Binary    (Binary (..))
import           Data.Default   (Default (..))
import           Data.Typeable  (Typeable)
import           GHC.Generics   (Generic)
import           Yi.Buffer      (getBufferDyn, putBufferDyn)
import           Yi.Types       (BufferM, YiVariable)

newtype DisplayLineNumbersLocal = DisplayLineNumbersLocal { unDisplayLineNumbersLocal :: Maybe Bool }
  deriving (Generic, Typeable)

instance Default DisplayLineNumbersLocal where
  def = DisplayLineNumbersLocal Nothing

instance Binary DisplayLineNumbersLocal

instance YiVariable DisplayLineNumbersLocal

-- | Get the buffer-local line number setting.
getDisplayLineNumbersLocal :: BufferM (Maybe Bool)
getDisplayLineNumbersLocal = unDisplayLineNumbersLocal <$> getBufferDyn

-- | Set the buffer-local line number setting.
-- Nothing: use global setting
-- Just True: display line numbers only in this buffer
-- Just False: hide line numbers only in this buffer
setDisplayLineNumbersLocal :: Maybe Bool -> BufferM ()
setDisplayLineNumbersLocal = putBufferDyn . DisplayLineNumbersLocal

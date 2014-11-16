{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
-- Module      :  Yi.Command.Help
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Help command support
-- This module uses Yi.Eval.describeNamedAction to
-- show whatever information about particular action is available
-- from current evaluator (ghciEvaluator currently presents only type.)
-- TODO: Would be nice to show excerpt from Haddock documentation in the future.
--
-- If given no arguments, the help index is shown (using @getAllNamesInScope@).
--
-- Please do not try to show descriptions for the whole index,
-- as our interface to GHCi is too slow.

module Yi.Command.Help(displayHelpFor) where

import           Control.Applicative
import           Data.Binary
import           Data.Default
import           Data.Typeable
import           Yi.Buffer
import           Yi.Editor
import           Yi.Monad
import qualified Yi.Rope as R
import qualified Data.Text as T
import           Yi.Eval (getAllNamesInScope, describeNamedAction)
import           Yi.Keymap
import           Yi.Types(YiVariable)

-- | Displays help for a given name, or help index, if no name is given
displayHelpFor :: T.Text -> YiM ()
displayHelpFor name = helpFor name >>= displayHelpBuffer

-- | Finds help text to display, given a command argument
helpFor :: T.Text -> YiM T.Text
helpFor ""    = (T.unlines . map T.pack) <$> getAllNamesInScope
helpFor name  = T.pack    <$> describeNamedAction (T.unpack name)          

-- * To make help buffer unique:
-- | Dynamic YiVariable to store the help buffer reference.
newtype HelpBuffer = HelpBuffer { helpBuffer :: Maybe BufferRef }
    deriving (Default, Typeable, Binary)

instance YiVariable HelpBuffer

-- | Display help buffer with a given text...
displayHelpBuffer :: T.Text -> YiM ()
displayHelpBuffer text = withEditor $ withOtherWindow $ do
   maybeM deleteBuffer =<< helpBuffer <$> getEditorDyn
   b <- newBufferE (MemBuffer "*help*") $ R.fromText text
   putEditorDyn $ HelpBuffer $ Just b

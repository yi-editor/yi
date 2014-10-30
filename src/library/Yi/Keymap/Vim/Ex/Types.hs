{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Types
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Types where

import Data.Maybe
import Data.Text (Text, unpack)
import Yi.Keymap
import Yi.Keymap.Vim.Common

data ExCommand = ExCommand {
    cmdComplete :: YiM [Text]
  , cmdIsPure :: Bool
  , cmdAction :: Action
  , cmdAcceptsRange :: Bool
  , cmdShow :: Text
}

instance Show ExCommand where
    show = unpack . cmdShow

data LineRange
    = MarkRange String String -- ^ 'a,'b
    | FullRange               -- ^ %
    | CurrentLineRange

evStringToExCommand :: [EventString -> Maybe ExCommand] -> EventString -> Maybe ExCommand
evStringToExCommand parsers s = listToMaybe . mapMaybe ($ s) $ parsers

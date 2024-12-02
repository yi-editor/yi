{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Copy
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- :copy ex command to copy selection to the clipboard.
module Yi.Keymap.Vim.Ex.Commands.Copy (parse) where

import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (match, string)
import           Data.Monoid                      ((<>))
import           Yi.Editor                        (withCurrentBuffer)
import           Yi.Keymap                        (Action (YiA))
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, impureExCommand, parseRange)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import           Yi.Keymap.Vim.Common             (EventString)
import           Yi.Types                         (YiM, BufferM)
import           Yi.Rope                          (toString)
import           Yi.Buffer.Region                 (readRegionB, Region)
import           Control.Monad.Base               (liftBase)
import           Yi.Clip                          (setClipboard)
import           Yi.Core                          (errorEditor)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    (regionText, region) <- P.match Common.parseRange
    void (P.string "copy")
    return $ Common.impureExCommand {
        cmdShow = regionText <> "copy"
      , cmdAction = YiA (copy region)
      }

copy :: Maybe (BufferM Region) -> YiM ()
copy maybeGetRegion = case maybeGetRegion of
    Nothing -> errorEditor "Cannot copy: No region"
    Just getRegion -> setClipboard
      =<< withCurrentBuffer (readRegionB =<< getRegion)

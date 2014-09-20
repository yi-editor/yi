{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Users.Anders
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Config.Users.Anders (config, main) where

import           Control.Lens
import           Data.Monoid
import           Yi hiding (Block)
import           Yi.Hoogle (hoogle)
import           Yi.Lexer.Haskell (TT)
import qualified Yi.Mode.Haskell as H
import           Yi.Modes (removeAnnots)
import qualified Yi.Rope as R
import           Yi.String (mapLines)
import           Yi.Syntax.Haskell (Tree)

config :: Config
config = defaultEmacsConfig

-- | Increase the indentation of the selection
increaseIndent :: BufferM ()
increaseIndent = do
  r <- getSelectRegionB
  r' <- unitWiseRegion Yi.Line r
     -- extend the region to full lines
  modifyRegionB (mapLines $ R.cons ' ') r'
     -- prepend each line with a space

main :: IO ()
main = yi $ config
  { defaultKm = defaultKm config
  , startFrontEnd = startFrontEnd config
  , modeTable =
    -- My precise mode with my hooks added
    AnyMode (haskellModeHooks H.preciseMode)
    -- My no annotations mode with mode hooks
    : AnyMode (haskellModeHooks noAnnots)
    : modeTable defaultConfig
  }

-- | Set my hooks for nice features
haskellModeHooks :: Mode syntax -> Mode syntax
haskellModeHooks mode =
   mode { modeName = "my " <> modeName mode
        , modeKeymap =
            topKeymapA %~ ((ctrlCh 'c' ?>>
                            choice [ ctrlCh 'l' ?>>! H.ghciLoadBuffer
                                   , ctrl (char 'z') ?>>! H.ghciGet
                                   , ctrl (char 'h') ?>>! hoogle
                                   , ctrlCh 'r' ?>>! H.ghciSend ":r"
                                   , ctrlCh 't' ?>>! H.ghciInferType
                                   , ctrlCh 'n' ?>>! increaseIndent
                                   ])
                           <||) }

-- This is used in order to remove the unicode characters usually used.
noAnnots :: Mode (Tree TT)
noAnnots = removeAnnots (H.preciseMode {modeName = "preciseNoUnicode"})

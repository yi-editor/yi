{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Compilation
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A 'Mode' for working with buffers showing the results of compilations.
module Yi.Mode.Compilation where

import           Control.Lens
import           Data.Text ()
import           Yi.Buffer
import           Yi.Core (withSyntax)
import           Yi.Editor
import           Yi.File (editFile)
import           Yi.Lexer.Alex (Tok(..), Posn(..))
import qualified Yi.Lexer.Compilation as Compilation
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Modes (styleMode, TokenBasedMode)
import qualified Yi.Syntax.OnlineTree as OnlineTree

mode :: TokenBasedMode Compilation.Token
mode = styleMode Compilation.lexer
  & modeAppliesA .~ modeNeverApplies
  & modeNameA .~ "compilation"
  & modeKeymapA .~ topKeymapA %~ ((spec KEnter ?>>! withSyntax modeFollow) <||)
  & modeFollowA .~ YiA . follow
  where
    follow errs = withCurrentBuffer pointB >>= \point ->
      case OnlineTree.tokAtOrBefore point errs of
        Just t@Tok {tokT = Compilation.Report filename line col _} -> do
          withCurrentBuffer . moveTo . posnOfs $ tokPosn t
          shiftOtherWindow
          _ <- editFile filename
          withCurrentBuffer $ gotoLn line >> rightN col
        _ -> return ()

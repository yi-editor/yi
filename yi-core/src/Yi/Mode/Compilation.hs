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

import           Control.Lens         ((%~), (&), (.~))
import           Data.Text            ()
import           Yi.Buffer
import           Yi.Core              (withSyntax)
import           Yi.Editor            (shiftOtherWindow, withCurrentBuffer)
import           Yi.File              (openingNewFile)
import           Yi.Keymap            (Action (YiA), topKeymapA)
import           Yi.Keymap.Keys       (Key (KEnter), spec, (<||), (?>>!))
import           Yi.Lexer.Alex        (Posn (..), Tok (..))
import qualified Yi.Lexer.Compilation as Compilation (Token (Report), lexer)
import           Yi.Modes             (TokenBasedMode, styleMode)
import qualified Yi.Syntax.OnlineTree as OnlineTree (tokAtOrBefore)

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
          openingNewFile filename $ gotoLn line >> rightN col
        _ -> return ()

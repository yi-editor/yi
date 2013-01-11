-- Copyright (c) Jean-Philippe Bernardy 2008 
module Yi.Mode.Compilation where

import Prelude ()
import Yi.Core
import Yi.File (editFile)
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Style
import Yi.Modes (linearSyntaxMode)
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Syntax.OnlineTree as OnlineTree

mode :: Mode (OnlineTree.Tree (Tok Compilation.Token))
mode = (linearSyntaxMode Compilation.initState Compilation.alexScanToken tokenToStyle)
  { 
   modeApplies = modeNeverApplies,
   modeName = "compilation",
   modeKeymap = topKeymapA %~ ((<||) (spec KEnter ?>>! withSyntax modeFollow)),
   modeFollow = \synTree -> YiA (follow synTree)
  }
    where tokenToStyle _ = commentStyle
          follow errs = do 
              point <- withBuffer pointB
              case OnlineTree.tokAtOrBefore point errs of
                 Just (t@Tok {tokT = Compilation.Report filename line col _message}) -> do
                     withBuffer $ moveTo $ posnOfs $ tokPosn $ t
                     shiftOtherWindow
                     discard $ editFile filename
                     withBuffer $ do 
                         discard $ gotoLn line
                         rightN col
                 _ -> return ()


-- Copyright (c) Jean-Philippe Bernardy 2008 
module Yi.Mode.Compilation where

import Prelude ()
import Yi.Core
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Style
import Yi.Syntax
import Yi.Dired
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Syntax.Linear as Linear

mode :: Mode (Linear.Result (Tok Compilation.Token))
mode = emptyMode
  { 
   modeApplies = const False,
   modeName = "compilation",
   modeKeymap = (<||) (spec KEnter ?>>! withSyntax modeFollow),
   modeFollow = \synTree -> YiA (follow synTree),
   modeHL = ExtHL $ mkHighlighter (Linear.incrScanner . Alex.lexScanner Compilation.alexScanToken Compilation.initState) 
                                  (\begin end pos -> Linear.getStrokes begin end pos . fmap tokenToStroke)
  }
    where tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)
          tokenToStyle _ = commentStyle
          follow :: Linear.Result (Tok Compilation.Token) -> YiM ()
          follow errs = do 
              point <- withBuffer pointB
              case Linear.tokAtOrBefore point errs of
                 Just (t@Tok {tokT = Compilation.Report filename line col _message}) -> do
                     withBuffer $ moveTo $ posnOfs $ tokPosn $ t
                     shiftOtherWindow
                     fnewE filename
                     withBuffer $ do 
                         gotoLn line
                         rightN col
                 _ -> return ()


module Yi.Mode.Compilation where

import Prelude ()
import Yi.Buffer
import Yi.Core
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import Yi.Dired
import Yi.Editor
import Yi.Keymap
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Compilation         as Compilation
import qualified Yi.Syntax.Linear as Linear
import Yi.Keymap.Keys

compilationMode :: Mode (Linear.Result (Tok Compilation.Token))
compilationMode = emptyMode
  { 
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
                 Just (Tok {tokT = Compilation.Report filename line col _message}) -> do
                     shiftOtherWindow
                     fnewE filename
                     withBuffer $ do 
                         gotoLn line
                         rightN col
                 _ -> return ()


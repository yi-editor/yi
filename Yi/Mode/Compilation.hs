module Yi.Mode.Compilation where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Yi.Buffer
import Yi.Buffer.HighLevel (fillParagraph)
import Yi.Core
import Yi.Indent
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Mode.Haskell
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
              case Linear.tokBefore point errs of
                 Nothing -> return ()
                 Just (Tok {tokT = Compilation.Report file line col _message}) -> do
                     shiftOtherWindow
                     fnewE file
                     withBuffer $ do 
                         gotoLn line
                         rightN col


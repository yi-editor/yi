module Yi.Modes (defaultFundamentalMode, fundamental,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode, defaultModeMap) where

import Control.Arrow (first)
import Prelude ()
import System.FilePath
import Yi.Buffer (AnyMode(..), Mode(..), emptyMode)
import Yi.Buffer.HighLevel (fillParagraph)
import Yi.Buffer.Indent
import Yi.Lexer.Alex (Tok(..), Posn(..))
import Yi.Mode.Haskell
import Yi.Prelude
import Yi.Style
import Yi.Syntax
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.Cabal               as Cabal
import qualified Yi.Lexer.Cplusplus           as Cplusplus
import qualified Yi.Lexer.Latex               as Latex
import qualified Yi.Lexer.LiterateHaskell     as LiterateHaskell
import qualified Yi.Lexer.OCaml               as OCaml
import qualified Yi.Lexer.Perl                as Perl
import qualified Yi.Lexer.Srmc                as Srmc
import qualified Yi.Syntax.Linear as Linear


fundamental, defaultFundamentalMode :: Mode syntax
latexMode, cppMode, literateHaskellMode, cabalMode, srmcMode, ocamlMode, perlMode :: Mode (Linear.Result Stroke)

fundamental = emptyMode
  { 
   modeIndent = \_ast -> autoIndentB,
   modePrettify = \_ -> fillParagraph
  }

mkHighlighter' :: forall token lexerState.
                    lexerState
                    -> (Alex.ASI lexerState
                        -> Maybe (Tok token, Alex.ASI lexerState))
                    -> (token -> StyleName)
                    -> Highlighter
                         (Yi.Syntax.Cache
                            (Alex.AlexState lexerState,
                             [Stroke])
                            (Linear.Result Stroke))
                         (Linear.Result Stroke)
mkHighlighter' initSt scan tokenToStyle = mkHighlighter (Linear.incrScanner . Alex.lexScanner (fmap (first tokenToStroke) . scan) initSt) Linear.getStrokes
    where tokenToStroke (Tok t len posn) = (posnOfs posn, tokenToStyle t, posnOfs posn +~ len)

cppMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' Cplusplus.initState Cplusplus.alexScanToken id
  }

literateHaskellMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' LiterateHaskell.initState LiterateHaskell.alexScanToken id
  }

cabalMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' Cabal.initState Cabal.alexScanToken id
  }

latexMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' Latex.initState Latex.alexScanToken id
  }

srmcMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' Srmc.initState Srmc.alexScanToken id
  }

ocamlMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' OCaml.initState OCaml.alexScanToken OCaml.tokenToStyle
  }

perlMode = fundamental
  {
    modeHL = ExtHL $ mkHighlighter' Perl.initState Perl.alexScanToken id
  }

defaultFundamentalMode = fundamental

defaultModeMap :: ReaderT FilePath Maybe AnyMode
defaultModeMap = ReaderT (modeFromExtension . takeExtension)




{-
  Working out the mode from the extension of
  the file. Some of these are a little questionably haskell
  related. For example ".x" is an alex lexer specification
  I dare say that there are other file types that use ".x"
  as the file extension.
  For now though this is probably okay given the users of
  'yi' are mostly haskell hackers, as of yet.
-}
modeFromExtension :: String -> Maybe AnyMode
modeFromExtension ".hs"    = Just $ AnyMode haskellMode
modeFromExtension ".x"     = Just $ AnyMode haskellMode
modeFromExtension ".lhs"   = Just $ AnyMode literateHaskellMode
modeFromExtension ".hsc"   = Just $ AnyMode haskellMode
modeFromExtension ".hsinc" = Just $ AnyMode haskellMode -- haskell include files such as Yi/Syntax/alex.hsinc
modeFromExtension ".cabal" = Just $ AnyMode cabalMode
modeFromExtension ".tex"   = Just $ AnyMode latexMode
modeFromExtension ".sty"   = Just $ AnyMode latexMode
modeFromExtension ".ltx"   = Just $ AnyMode latexMode
modeFromExtension ".cxx"   = Just $ AnyMode cppMode
modeFromExtension ".cpp"   = Just $ AnyMode cppMode
modeFromExtension ".C"     = Just $ AnyMode cppMode
modeFromExtension ".hxx"   = Just $ AnyMode cppMode
modeFromExtension ".H"     = Just $ AnyMode cppMode
modeFromExtension ".h"     = Just $ AnyMode cppMode
modeFromExtension ".c"     = Just $ AnyMode cppMode -- I treat c file as cpp files, most users are smart enough to allow for that.
modeFromExtension ".pepa"  = Just $ AnyMode srmcMode -- pepa is a subset of srmc    
modeFromExtension ".pl"    = Just $ AnyMode perlMode
modeFromExtension ".pm"    = Just $ AnyMode perlMode
modeFromExtension ".t"     = Just $ AnyMode perlMode
modeFromExtension ".srmc"  = Just $ AnyMode srmcMode
modeFromExtension ".ml"    = Just $ AnyMode ocamlMode
modeFromExtension ".mli"   = Just $ AnyMode ocamlMode
modeFromExtension ".mly"   = Just $ AnyMode ocamlMode
modeFromExtension ".mll"   = Just $ AnyMode ocamlMode
modeFromExtension ".ml4"   = Just $ AnyMode ocamlMode
modeFromExtension ".mlp4"  = Just $ AnyMode ocamlMode
modeFromExtension _        = Nothing

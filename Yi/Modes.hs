module Yi.Modes (defaultFundamentalMode,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode, defaultModeMap) where

import System.FilePath
import qualified Yi.Syntax.Haskell             as Haskell
import qualified Yi.Syntax.LiterateHaskell     as LiterateHaskell
import qualified Yi.Syntax.Latex               as Latex
import qualified Yi.Syntax.Srmc                as Srmc
import qualified Yi.Syntax.Cabal               as Cabal
import qualified Yi.Syntax.Cplusplus           as Cplusplus
import qualified Yi.Syntax.Fractal as Fractal
import qualified Yi.Syntax.Alex as Alex
import Yi.Syntax
import Yi.Keymap
import Yi.Indent
import Control.Arrow (first)
import Yi.Prelude
import Prelude ()

fundamental, defaultFundamentalMode,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode :: Mode

fundamental = Mode 
  { 
   modeHL = ExtHL noHighlighter,
   modeKeymap = id,
   modeIndent = autoIndentB
  }

cppMode = fundamental 
  {
   modeHL = ExtHL (Alex.mkHighlighter Cplusplus.initState Cplusplus.alexScanToken)
  }

haskellMode = fundamental 
   {
    modeHL = 
    ExtHL (Alex.mkHighlighter Haskell.initState (fmap (first tokenToStroke) . Haskell.alexScanToken))
    -- ExtHL (Fractal.mkHighlighter Haskell.initState Haskell.alexScanToken tokenToStroke)
   , modeIndent = autoIndentHaskellB
   }
    where tokenToStroke (l,t,r) = (l,Haskell.tokenToStyle t,r)

literateHaskellMode = haskellMode 
  {
   modeHL = ExtHL (Alex.mkHighlighter LiterateHaskell.initState LiterateHaskell.alexScanToken)
  }

cabalMode = fundamental 
  {
    modeHL = ExtHL (Alex.mkHighlighter Cabal.initState Cabal.alexScanToken)
  }

latexMode = fundamental 
  {
   modeHL = ExtHL (Alex.mkHighlighter Latex.initState Latex.alexScanToken)
  }

srmcMode = fundamental 
   {
    modeHL = ExtHL (Alex.mkHighlighter Srmc.initState Srmc.alexScanToken)
   }

defaultFundamentalMode = fundamental

defaultModeMap :: ReaderT FilePath Maybe Mode
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
modeFromExtension :: String -> Maybe Mode
modeFromExtension ".hs"    = Just haskellMode
modeFromExtension ".x"     = Just haskellMode
modeFromExtension ".lhs"   = Just literateHaskellMode
modeFromExtension ".hsinc" = Just haskellMode -- haskell include files such as Yi/Syntax/alex.hsinc
modeFromExtension ".cabal" = Just cabalMode
modeFromExtension ".tex"   = Just latexMode
modeFromExtension ".sty"   = Just latexMode
modeFromExtension ".ltx"   = Just latexMode
modeFromExtension ".cxx"   = Just cppMode
modeFromExtension ".cpp"   = Just cppMode
modeFromExtension ".C"     = Just cppMode
modeFromExtension ".hxx"   = Just cppMode
modeFromExtension ".H"     = Just cppMode
modeFromExtension ".h"     = Just cppMode
modeFromExtension ".c"     = Just cppMode -- I treat c file as cpp files, most users are smart enough to allow for that.
modeFromExtension ".pepa"  = Just srmcMode -- pepa is a subset of srmc    
modeFromExtension ".srmc"  = Just srmcMode
modeFromExtension _        = Nothing

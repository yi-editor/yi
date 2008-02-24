module Yi.Modes (defaultFundamentalMode,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode, defaultModeMap) where

import System.FilePath
import qualified Yi.Syntax.Haskell
import qualified Yi.Syntax.LiterateHaskell (highlighter)
import qualified Yi.Syntax.Latex
import qualified Yi.Syntax.Srmc
import qualified Yi.Syntax.Cabal
import qualified Yi.Syntax.Cplusplus
import qualified Yi.Syntax.Fractal as Fractal
import Yi.Syntax
import Yi.Keymap
import Yi.Indent
{- End of Imports -}

fundamental, defaultFundamentalMode,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode :: Mode

fundamental = Mode 
  { 
   modeHL = ExtHL noHighlighter,
   modeKeymap = id,
   modeIndent = autoIndentB
  }

cppMode = fundamental {
     modeHL = ExtHL (Yi.Syntax.Cplusplus.highlighter)
                          }

haskellMode = fundamental {
     modeHL = ExtHL (Yi.Syntax.Haskell.highlighter)
     -- ExtHL (Fractal.mkHighlighter Yi.Syntax.Haskell.initState Yi.Syntax.Haskell.alexScanToken))
     , modeIndent = autoIndentHaskellB
                          }

literateHaskellMode = haskellMode {
     modeHL = ExtHL (Yi.Syntax.LiterateHaskell.highlighter)
                                  }
cabalMode = fundamental {
     modeHL = ExtHL (Yi.Syntax.Cabal.highlighter)
}


latexMode = fundamental {
                             modeHL = ExtHL (Yi.Syntax.Latex.highlighter)
            }

srmcMode = fundamental {
                             modeHL = ExtHL (Yi.Syntax.Srmc.highlighter)
           }

defaultFundamentalMode = fundamental

defaultModeMap :: FilePath -> Maybe Mode
defaultModeMap = modeFromExtension . takeExtension




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

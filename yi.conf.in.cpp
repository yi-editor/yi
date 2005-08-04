#if GLASGOW_HASKELL < 604
Package {
        name            = "yi",
        auto            = False,
        hs_libraries    = [ "HSyi" ],
#ifdef INSTALLING
        import_dirs     = [ "${PREFIX}/lib/yi/imports" ],
        library_dirs    = [ "${PREFIX}/lib/yi" ],
#else
        import_dirs     = [ "${PREFIX}" ],
        library_dirs    = [ "${PREFIX}" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [ "${CURSES}" ],
        package_deps    = [ "base", "haskell98", "posix" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}

#else

name:           yi
version:        0.1
license:        LGPL
maintainer:     dons@cse.unsw.edu.au
exposed:        True
exposed-modules:
        Yi.Buffer
        Yi.Char
        Yi.CharMove
        Yi.Core
        Yi.Curses.CWString
        Yi.Curses.Curses
        Yi.Curses.UI
        Yi.DLists
        Yi.Editor
        Yi.FastBuffer
        Yi.Keymap.Completion
        Yi.Keymap.Ee
        Yi.Keymap.Emacs
        Yi.Keymap.Emacs.Keys
        Yi.Keymap.Emacs.KillRing
        Yi.Keymap.Emacs.UnivArgument
        Yi.Keymap.Emacs2
        Yi.Keymap.Joe
        Yi.Keymap.Mg
        Yi.Keymap.Nano
        Yi.Keymap.Vi
        Yi.Keymap.Vim
        Yi.Lexers
        Yi.Locale
        Yi.MakeKeymap
        Yi.Map
        Yi.MkTemp
        Yi.Process
        Yi.Regex
        Yi.String
        Yi.Style
        Yi.Syntax.Syntax
        Yi.Syntax.TestLex
        Yi.Syntax.TestParse
        Yi.Undo
        Yi.Version
        Yi.Window
        Yi.Yi

hidden-modules:
#ifdef INSTALLING
import-dirs:          PREFIX/lib/yi/imports
library-dirs:         PREFIX/lib/yi
#else
import-dirs:          PREFIX
library-dirs:         PREFIX
#endif
hs-libraries:         HSyi
extra-libraries:      CURSES
include-dirs:
includes:             
depends:              base haskell98 mtl
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:

#endif

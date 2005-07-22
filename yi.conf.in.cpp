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
        extra_libraries = [ "${CURSES}", "${ICONV}" ],
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
        Yi.CharMove
        Yi.Char
        Yi.Core
        Yi.Curses
        Yi.DLists
        Yi.Editor
        Yi.FastBuffer
        Yi.Keymap
        Yi.Lexers
        Yi.Locale
        Yi.Map
        Yi.MkTemp
        Yi.MakeKeymap
        Yi.Regex
        Yi.Style
        Yi.Undo
        Yi.Version
        Yi.Window
        Yi.Yi
        Yi.Curses.CWString
        Yi.Curses.Curses
        Yi.Curses.IConv
        Yi.Curses.UI
        Yi.Keymap.Emacs
        Yi.Keymap.Emacs2
        Yi.Keymap.Mg
        Yi.Keymap.Nano
        Yi.Keymap.Vi
        Yi.Keymap.Vim
        Yi.Keymap.Joe
        Yi.Keymap.Ee

hidden-modules:
#ifdef INSTALLING
import-dirs:          PREFIX/lib/yi/imports
library-dirs:         PREFIX/lib/yi
#else
import-dirs:          PREFIX
library-dirs:         PREFIX
#endif
hs-libraries:         HSyi
extra-libraries:      CURSES ICONV
include-dirs:
includes:             
depends:              base haskell98
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:

#endif

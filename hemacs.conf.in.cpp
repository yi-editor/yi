Package {
        name            = "hemacs",
        auto            = False,
        hs_libraries    = [ "HShemacs" ],
#ifdef INSTALLING
        import_dirs     = [ "${PREFIX}/lib/hemacs/imports" ],
        library_dirs    = [ "${PREFIX}/lib/hemacs" ],
#else
        import_dirs     = [ "${PREFIX}" ],
        library_dirs    = [ "${PREFIX}" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [],
        extra_libraries = [ "${CURSES}", "iconv" ],
        package_deps    = [ "base", "haskell98" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}

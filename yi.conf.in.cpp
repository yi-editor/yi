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

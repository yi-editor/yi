AC_INIT(Yi/Yi.hs)

if test ! -f .setup-config; then
    exec runhaskell Setup.hs configure "$@"
fi

AC_CONFIG_SRCDIR([yi.cabal])
AC_CONFIG_HEADERS([cbits/config.h])
AC_PREREQ([2.50])

AC_CHECK_PROGS(M4, [gm4 m4], [false])
AC_CHECK_LIB(curses,  addnstr)
AC_CHECK_HEADERS([regex.h signal.h], [], [AC_MSG_ERROR([Your system is missing a required header.])])
AC_CHECK_FUNCS(use_default_colors)

PLATFORM_=`uname -ms`
AC_DEFINE_UNQUOTED(PLATFORM, "$PLATFORM_", [Platform we are on])

if test -d _darcs; then
AC_CHECK_PROG(DARCS, darcs, darcs)
# Need to do darcs changes --context!
fi

# for the testsuite
AC_CONFIG_FILES([testsuite/pp/logpp])
AC_CONFIG_COMMANDS([default], [chmod +x testsuite/pp/logpp] )
AC_OUTPUT

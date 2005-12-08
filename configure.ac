AC_INIT(Yi/Yi.hs)

if test ! -f .setup-config; then
    exec runhaskell Setup.hs configure "$@"
fi

AC_CONFIG_SRCDIR([yi.cabal])
AC_CONFIG_HEADERS([cbits/config.h])
AC_PREREQ([2.50])

AC_CHECK_PROGS(M4, [gm4 m4], [false])

CURSES=""
AC_CHECK_LIB(ncursesw, waddnwstr, [
    AC_CHECK_HEADER(ncursesw/ncurses.h, [
        AC_DEFINE([HAVE_NCURSESW], [], [ncurses with wide chars is available])
        AC_DEFINE([HAVE_NCURSES], [], [ncurses is available])
        CURSES=ncursesw
    ])
])
if test -z "$CURSES"; then
    AC_CHECK_LIB(ncurses, addnstr, [
        AC_CHECK_HEADER(ncurses.h, [
            AC_DEFINE(HAVE_NCURSES)
            CURSES=ncurses
        ])
    ])
fi
if test -z "$CURSES"; then
    AC_CHECK_LIB(curses, addnstr, [
        AC_CHECK_HEADER(curses.h, [CURSES=curses])
    ])
fi
if test -z "$CURSES"; then
    AC_MSG_ERROR([Unable to locate a curses library.])
fi
AC_SUBST(CURSES)
LIBS="-l$CURSES $LIBS"

AC_CHECK_HEADERS([sys/types.h regex.h locale.h langinfo.h wchar.h limits.h signal.h], [], [AC_MSG_ERROR([Your system is missing a required header.])])

AC_CHECK_FUNCS([arc4random resizeterm])

PLATFORM_=`uname -ms`
AC_DEFINE_UNQUOTED(PLATFORM, "$PLATFORM_", [Platform we are on])

if test -d _darcs; then
AC_CHECK_PROG(DARCS, darcs, darcs)
# Need to do darcs changes --context!
fi

AC_CONFIG_FILES([testsuite/pp/logpp yi.buildinfo])
AC_CONFIG_COMMANDS([default], [chmod +x testsuite/pp/logpp] )
AC_OUTPUT

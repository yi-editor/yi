
# sanity
AC_INIT(Yi/Yi.hs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([mk/config.h])

# this is arbitrary
AC_PREREQ([2.52])

#
# Looking for GHC, stolen from $fptools
#
AC_ARG_WITH(ghc,
[AC_HELP_STRING([--with-ghc=ARG],
    [Use ARG as the path to GHC  [default=autodetect]])],
[ WithGhc="$withval" ],
[
  if test "$GHC" = ""; then
    AC_PATH_PROG(GHC,ghc)
  fi
  WithGhc=$GHC
])
AC_SUBST(WithGhc)

#
# Look for a ghc-pkg that matches our ghc version.
#
ghc_pkg_guess=`echo $WithGhc | sed 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$ghc_pkg_guess" -l > /dev/null 2>&1; then
      matching_ghc_pkg=$ghc_pkg_guess
else
      matching_ghc_pkg=no
fi
if test x"$matching_ghc_pkg" = xno; then
    AC_PATH_PROG([GhcPkgCmd], [ghc-pkg])
else
    GhcPkgCmd=$matching_ghc_pkg
fi
AC_SUBST(GhcPkgCmd)

#
# And some other tools
#
AC_PATH_PROG(AlexCmd,alex,alex)
#AC_PATH_PROG(HappyCmd,happy,happy)
AC_PATH_PROG(HaddockCmd,haddock,haddock)
AC_PATH_PROG(Hsc2hsCmd,hsc2hs,hsc2hs)

#
# May as well check for this
#
AC_PROG_RANLIB

#
# Curses. If you don't have wchar_t-aware ncurses (Debian package:
# libncursesw5-dev),  uncomment the first line and comment-out the
# two later. 
#
# The shell variables should be added to, e.g., $LIBS
#
AC_CHECK_LIB(curses,  addnstr)
AC_CHECK_LIB(ncurses, addnstr)
AC_CHECK_LIB(ncursesw,waddnwstr)

AC_CHECK_LIB(iconv, iconv)
AC_CHECK_LIB(iconv, libiconv)

# Bit weird. On OpenBSD you need the 'lib' suffix to iconv functions, it seems
AC_CHECK_FUNCS(libiconv, AC_DEFINE( [ICONV_LIB_PREFIX],[1],
                [Define to 1 if you need a 'lib' prefix to iconv functions]))

# some special libs need to be set for package.conf files
if echo "$LIBS" | grep iconv > /dev/null 2>&1 ; then
    ICONV=iconv
fi

if (echo "$LIBS" | grep ncursesw) >/dev/null 2>&1 ; then
    CURSES=ncursesw
elif (echo "$LIBS" | grep ncurses) >/dev/null 2>&1 ; then
    CURSES=ncurses
else
    CURSES=curses
fi
AC_SUBST(ICONV)
AC_SUBST(CURSES)

#
# Specific headers to check for
#
AC_CHECK_HEADERS([iconv.h curses.h ncurses.h ncursesw/ncurses.h regex.h locale.h langinfo.h wchar.h limits.h ])

#
# We also would like this please
# 
AC_CHECK_FUNCS(arc4random)

AC_CONFIG_FILES([mk/config.mk])
AC_OUTPUT

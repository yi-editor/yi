
# sanity
AC_INIT(Yi/Yi.hs)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([mk/config.h])

# this is arbitrary
AC_PREREQ([2.52])

#
# Looking for HC, stolen from $fptools
#
AC_ARG_WITH(hc,
[AC_HELP_STRING([--with-hc=ARG],
    [Use ARG as the path to a Haskell compiler [default=autodetect]])],
[ WithGhc="$withval" 
  if test "$WithGhc" = "nhc98"; then
    AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
    SYMS="$SYMS -D__NHC__"
  fi
],
[
  if test "$GHC" = ""; then
    AC_CHECK_PROGS(GHC,ghc)
  fi
  if test "$GHC" = ""; then
    AC_CHECK_PROGS(GHC,nhc98)
  fi
  if test "$GHC" = "nhc98"; then
    AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
    SYMS="$SYMS -D__NHC__"
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
    AC_CHECK_PROG([GhcPkgCmd], [ghc-pkg])
else
    GhcPkgCmd=$matching_ghc_pkg
fi
AC_SUBST(GhcPkgCmd)

#
# And some other tools
#
AC_CHECK_PROG(AlexCmd,alex,alex)
#AC_PATH_PROG(HappyCmd,happy,happy)
AC_CHECK_PROG(HaddockCmd,haddock,haddock)
AC_CHECK_PROG(Hsc2hsCmd,hsc2hs,hsc2hs)

AC_CHECK_PROG(STRIP,strip,strip)
AC_PATH_PROG(LdCmd,ld)

# check for -x flag to ld. stolen from $fptools
AC_DEFUN(FPTOOLS_LD_X,
[AC_CACHE_CHECK([whether ld understands -x], fptools_cv_ld_x,
[
echo 'foo() {}' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r -x -o foo.o conftest.o; then
   fptools_cv_ld_x=yes 
else
   fptools_cv_ld_x=no
fi
rm -rf conftest.c conftest.o foo.o 
])
if test "$fptools_cv_ld_x" = yes; then
    LdXFlag=-x
else
    LdXFlag=
fi
AC_SUBST(LdXFlag)
])

FPTOOLS_LD_X

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

AC_SUBST(SYMS)

AC_CONFIG_FILES([mk/config.mk])
AC_OUTPUT

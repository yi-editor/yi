
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
[AC_HELP_STRING(
 	[--with-hc=ARG],
	[Use ARG as the path to a Haskell compiler [default=autodetect]])
],
[ WithGhc="$withval" 
  if test "$WithGhc" = "nhc98"; then
	AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
	SYMS="$SYMS -D__NHC__"
  fi
],
[
  # else check if --with-ghc was used
  AC_ARG_WITH(ghc,
  [AC_HELP_STRING(
   	[--with-ghc=ARG],
	[Use ARG as the path to a Haskell compiler [default=autodetect]])
  ],
  [ WithGhc="$withval" 
    if test "$WithGhc" = "nhc98"; then
	  AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
	  SYMS="$SYMS -D__NHC__"
    fi
  ],
  [
     # else autodetect
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
])
AC_SUBST(WithGhc)

#
# Look for a ghc-pkg that matches our ghc version.
#

AC_MSG_CHECKING([for ghc-pkg])
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
AC_MSG_RESULT([$GhcPkgCmd])
AC_SUBST(GhcPkgCmd)

#
# And look for an hsc2hs in the same place.
#
AC_MSG_CHECKING([for hsc2hs])
hsc2hs_guess=`echo $WithGhc | sed 's/ghc.*$/hsc2hs/'`
if "$hsc2hs_guess" -V > /dev/null 2>&1; then
      matching_hsc2hs=$hsc2hs_guess
else
      matching_hsc2hs=no
fi
if test x"$matching_hsc2hs" = xno; then
    AC_CHECK_PROG(Hsc2hsCmd,hsc2hs,hsc2hs)
else
    Hsc2hsCmd=$matching_hsc2hs
fi
AC_MSG_RESULT([$Hsc2hsCmd])
AC_SUBST(Hsc2hsCmd)

# check ghc version here
if test -n "$WithGhc" ; then
    AC_MSG_CHECKING([for ghc version])
    GHC_VERSION=`$WithGhc --numeric-version`
    AC_MSG_RESULT([$GHC_VERSION])
fi
AC_SUBST(GHC_VERSION)
AC_DEFINE_UNQUOTED(GHC_VERSION, "$GHC_VERSION", [GHC version])

# Work out value of __GLASGOW_HASKELL__
# Don't use ghci, as that's non-portable
if test -n "$WithGhc" ; then
 AC_MSG_CHECKING([for value of __GLASGOW_HASKELL__])
 echo "main = print __GLASGOW_HASKELL__" > t.hs
 GLASGOW_HASKELL=`"$WithGhc" -v0 -cpp t.hs > /dev/null 2>&1 && ./a.out`
 rm -f t.hs t.hi t.o a.out
 AC_MSG_RESULT([$GLASGOW_HASKELL])
fi
AC_SUBST(GLASGOW_HASKELL)

#
# And some other tools
#
AC_PATH_PROG(AlexCmd,alex)
if test -z "$AlexCmd"; then
      echo "You must install alex before you can continue"
      echo "Perhaps it is already installed, but not in your PATH?"
      exit 1
fi

AC_PATH_PROG(HappyCmd,happy)
if test -z "$HappyCmd"; then
      echo "You must install happy before you can continue"
      echo "Perhaps it is already installed, but not in your PATH?"
      exit 1
fi

AC_CHECK_PROG(HaddockCmd,haddock,haddock)

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
AC_CHECK_LIB(ncursesw,waddnwstr)
if echo "$LIBS" | grep ncursesw > /dev/null 2>&1 ; then
    :
else
    AC_CHECK_LIB(ncurses, addnstr)
    if echo "$LIBS" | grep ncurses > /dev/null 2>&1 ; then
        :
    else
        AC_CHECK_LIB(curses,  addnstr)
    fi
fi

AC_CHECK_LIB(iconv, iconv)
AC_CHECK_LIB(iconv, libiconv)

# Bit weird. On OpenBSD you need the 'lib' suffix to iconv functions, it seems
AC_CHECK_FUNCS(libiconv, AC_DEFINE( [ICONV_LIB_PREFIX],[1],
                [Define to 1 if you need a 'lib' prefix to iconv functions]))

# On linux we need to call doupdate() after endwin() to restore the term
if uname -s | grep Linux > /dev/null 2>&1 ; then
    AC_DEFINE( [NCURSES_UPDATE_AFTER_END],[1],
               [Define to 1 if you need to call doupdate after endwin])
fi

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
AC_CHECK_HEADERS([iconv.h curses.h ncurses.h ncursesw/ncurses.h regex.h locale.h langinfo.h wchar.h limits.h signal.h ])

#
# We also would like this please
# 
AC_CHECK_FUNCS(arc4random)

PLATFORM_=`uname -ms`
AC_DEFINE_UNQUOTED(PLATFORM, "$PLATFORM_", [Platform we are on])

REPO_PATH_=`sed -n 1p _darcs/prefs/repos`
AC_DEFINE_UNQUOTED(REPO_PATH, "$REPO_PATH_", [Path to Yi repo])

PATCH_COUNT_=`darcs changes --xml-output | sed -n '/^<patch/p;/TAG/q' | wc -l | sed 's/ *//g'`
AC_DEFINE_UNQUOTED(PATCH_COUNT, $PATCH_COUNT_, [Current patch count])

AC_SUBST(SYMS)

AC_CONFIG_FILES([mk/config.mk])
AC_OUTPUT

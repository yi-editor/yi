#
# System-specific settings
#

PREFIX=         /usr/local
BINDIR=         $(PREFIX)/bin
LIBDIR=         $(PREFIX)/lib

GHC=            ghc
GHC_PKG=        ghc-pkg
HC_OPTS=       -cpp -fglasgow-exts

#HC_OPTS+=      -O -fvia-C -funbox-strict-fields
#HC_OPTS+=      -Wall
#HC_OPTS+=      -debug

HSC2HS=         hsc2hs

AR=             ar
AR_OPTS=        cr

RANLIB=		ranlib

INSTALL=        sh $(TOPDIR)/install-sh -c
BIN_MODE=       755
LIB_MODE=       644

INSTALLDIR=     mkdir -p

RM=             rm -f

#
# Curses. If you don't have wchar_t-aware ncurses (Debian package:
# libncursesw5-dev),  uncomment the first line and comment-out the
# two later. 
#

CURSES 		 = ncurses
DEFINES 	+= -DHAVE_NCURSES_H=1
GHC_PKG_DEFINES += -DCURSES=$(CURSES)

#CURSES = ncursesw
#DEFINES += -DHAVE_NCURSESW_NCURSES_H=1 -DHAVE_WADDNWSTR=1

#
# Character sets. If your iconv implementation is lacking (glibc is ok),
# comment out the CF_CHARSET_SUPPORT line. If you system doesn't support 
# wchar_t or wchar_t isn't iso-10646, comment-out the CF_WCHAR_SUPPORT line.
# Usually, if you do one of these, you want to do the other as well, as the
# first option controls conversion between mbox and internal Unicode 
# presentation, and the second from Unicode to locale's encoding for display.
# If you unset CF_WCHAR_SUPPORT, you also should not use ncursesw above.
# Comment these out on OpenBSD
#
 
DEFINES += -DCF_CHARSET_SUPPORT
DEFINES += -DCF_WCHAR_SUPPORT

#
# Set this if your iconv library functions are accessed with the "lib"
# prefix. OpenBSD needs this.
#
#ICONV=          iconv
#DEFINES+=       -DWEIRD_ICONV

#
# Extra paths to find things
#
#LD_OPTS         += -L/usr/local/lib
#HC_OPTS         += -I/usr/local/include

#
# define this if you aren't on a posix machine (doesn't work yet)
#
#DEFINES += -DNO_POSIX 

#
# riot system.mk
#

PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib

#
# define this if you aren't on a posix machine (doesn't work yet)
#
#DEFINES += -DNO_POSIX 

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
#
 
# comment these out on OpenBSD
DEFINES += -DCF_CHARSET_SUPPORT
DEFINES += -DCF_WCHAR_SUPPORT

#LIBS_ICONV =

# OpenBSD needs:
#DEFINES    += -DWEIRD_ICONV
#LIBS_ICONV = -liconv -L/usr/local/lib

#
# Set this if you need extra flags to find libraries (for example,
# iconv might live in /usr/local, so you need :
#
#EXTRA_LIB_FLAGS=
#EXTRA_LIB_FLAGS=-I/usr/local/include

#
# OpenBSD specific settings. Turn all these on if you're on OpenBSD
#
#LIBS_ICONV      = -liconv -L/usr/local/lib
#EXTRA_LIB_FLAGS =-I/usr/local/include
#DEFINES         += -DWEIRD_ICONV

#
# GHC settings
#

GHC = ghc
GHC_PKG = ghc-pkg

# TODO: remove posix dependency, so it runs on windows
GHC_COMPILE_FLAGS = $(DEFINES) \
	-cpp -fglasgow-exts \
	-Icbits $(EXTRA_LIB_FLAGS) 

#GHC_OPT_FLAGS = -O -fvia-C -funbox-strict-fields
#GHC_WARNINGS  = -Wall

GHC_COMPILE_FLAGS += $(GHC_OPT_FLAGS)
GHC_COMPILE_FLAGS += $(GHC_WARNINGS)

# set this if you wish to debug the rts (hopefully never need to)
# GHC_COMPILE_FLAGS += -debug

GHC_C_COMPILE_FLAGS = $(GHC_COMPILE_FLAGS)

HSC2HS=hsc2hs $(DEFINES) $(EXTRA_LIB_FLAGS)

GHC_LINK_FLAGS = -package posix

#
# Misc. programs
#

AR = ar
ARFLAGS = cr

INSTALL = sh $(TOPDIR)/install-sh -c
BIN_MODE = 755
LIB_MODE = 644

INSTALLDIR = mkdir -p

RM = rm -f


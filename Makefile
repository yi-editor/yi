#
# hemacs Makefile
#

TOPDIR = .

include $(TOPDIR)/mk/config.mk

# this rule must remain first
default: boot all

ALL_DIRS=       HEmacs cbits

BIN=            hemacs
STATIC_BIN=     hemacs-static

ifeq "$(way)" ""
HS_BINS=        $(BIN) $(STATIC_BIN)
else
HS_BINS=        $(STATIC_BIN)
endif

# Library specific stuff
 
PKG=            hemacs

# dynamic front end
 
BIN_OBJS=       Boot.o
BIN_DEPS=       plugins
BIN_LIBS=       $(CURSES) $(ICONV)
HADDOCK_SRCS+=  Boot.hs

# static front end
 
STATIC_OBJS=    Main.o
STATIC_BIN_DEPS=hemacs
STATIC_HC_OPTS  += -package-conf hemacs.conf
HADDOCK_SRCS+=  Main.hs

# frontend to the library (by which it is loaded)

FRONTEND_HS_SRC=HEmacs.hs
LIB_FRONTEND=   HEmacs.$(way_)o
LIB_IFACE   =   HEmacs.$(way_)hi
HADDOCK_SRCS+=  $(FRONTEND_HS_SRC)
EXTRA_CLEANS+=	$(LIB_FRONTEND) $(LIB_IFACE)

# filter out recursive module
NO_DOCS+=       HEmacs/Config.hs

#
# read in suffix rules
#
include $(TOPDIR)/mk/rules.mk

#
# Special targets (just those in $(TOP))
# 

#
# Boot is the bootstrap loader. It cant be linked *statically* against -package hemacs.
#
Boot.o: Boot.hs 
	$(GHC) $(HC_OPTS) $(BIN_HC_OPTS) $(DEFINES) -main-is Boot.main -c $< -o $@ -ohi $(basename $@).$(way)hi

#
# Main is the static "loader". It can't get -package-name hemacs, or it
# won't work in ghci. Could probably filter it out somehow
#
Main.o: Main.hs 
	$(GHC) $(HC_OPTS) $(STATIC_HC_OPTS) -c $< -o $@ -ohi $(basename $@).hi

#
# HEmacs.o is the actual HEmacs.main, as well as being the frontend of
# the statically linked binary
#
# Semi-magic to defeat <= ghc-6.2.1 use of -i. by default. this stops
# us using a library and it's .o files easily in the same dir -- the
# .o files will always be used over the package dependency. Not an
# issue in ghc-6.2.2. Anyway, the Solution: cd somewhere where -i.
# means nothing.
#
MAGIC_FLAGS   += -package-conf ../hemacs.conf  -package hemacs

HEmacs.$(way_)o: HEmacs.hs
	( cd HEmacs ; $(GHC) $(HC_OPTS) $(MAGIC_FLAGS) -odir .. -c ../$< -o ../$@ -ohi ../$(basename $@).$(way_)hi )

hemacs-inplace: hemacs-inplace.in
	sed 's,@HEMACS_TOP@,'`pwd`',g' hemacs-inplace.in > hemacs-inplace
	chmod 755 hemacs-inplace

EXTRA_CLEANS+= hemacs-inplace

# Dependency orders

include $(TOPDIR)/depend

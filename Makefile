#
# yi Makefile
#

TOPDIR = .

include $(TOPDIR)/mk/config.mk

# this rule must remain first
default: boot all

ALL_DIRS=       Yi Yi/Keymap Yi/Ctk cbits

ifneq "$(CURSES)" ""
ALL_DIRS+=      Yi/Curses
endif

BIN=            yi_
STATIC_BIN=     yi-static

ifeq "$(way)" ""
HS_BINS=        $(BIN) $(STATIC_BIN)
else
HS_BINS=        $(STATIC_BIN)
endif

# Library specific stuff
 
PKG=            yi

# dynamic front end
 
BIN_OBJS=       Boot.o
BIN_DEPS=       plugins
BIN_LIBS=       $(LIBS)
HADDOCK_SRCS+=  Boot.hs

# static front end
 
STATIC_OBJS=    Main.o
STATIC_BIN_DEPS=yi
STATIC_HC_OPTS  += -package-conf yi.conf
HADDOCK_SRCS+=  Main.hs

# frontend to the library (by which it is loaded)

FRONTEND_HS_SRC=Yi.hs
LIB_FRONTEND=   Yi.$(way_)o
LIB_IFACE   =   Yi.$(way_)hi
HADDOCK_SRCS+=  $(FRONTEND_HS_SRC)
EXTRA_CLEANS+=	$(LIB_FRONTEND) $(LIB_IFACE)

# Must filter out circular dependencies
NO_DOCS=	Yi/Undo.hs

#
# read in suffix rules
#
include $(TOPDIR)/mk/rules.mk

#
# Special targets (just those in $(TOP))
# 

BIN_HC_OPTS+=     -DLIBDIR=\"$(LIBDIR)\"

#
# Boot is the bootstrap loader. It cant be linked *statically* against -package yi.
#
Boot.o: Boot.hs 
	$(GHC) $(HC_OPTS) $(BIN_HC_OPTS) -main-is Boot.main -c $< -o $@ -ohi $(basename $@).$(way_)hi

#
# Main is the static "loader". It can't get -package-name yi, or it
# won't work in ghci. Could probably filter it out somehow
#
Main.o: Main.hs Yi.o $(LIBRARY) 
	$(GHC) $(HC_OPTS) $(STATIC_HC_OPTS) -c $< -o $@ -ohi $(basename $@).hi

# Break some mutual recursion (why doesn't this work in mk/rules.mk??)
ifeq "$(GLASGOW_HASKELL)" "604"
%.$(way_)hi-boot : %.$(way_)o-boot
	@:
endif

#
# Yi.o is the actual Yi.main, as well as being the frontend of
# the statically linked binary
#
# Semi-magic to defeat <= ghc-6.2.1 use of -i. by default. this stops
# us using a library and it's .o files easily in the same dir -- the
# .o files will always be used over the package dependency. Not an
# issue in ghc-6.2.2. Anyway, the Solution: cd somewhere where -i.
# means nothing.
#
MAGIC_FLAGS   += -package-conf ../yi.conf  -package yi

Yi.$(way_)o: Yi.hs $(LIBRARY) 
	( cd Yi ; $(GHC) $(HC_OPTS) $(MAGIC_FLAGS) -I../cbits -odir .. -c ../$< -o ../$@ -ohi ../$(basename $@).$(way_)hi )

yi-inplace: yi-inplace.in
	@sed 's,@YI_TOP@,'`pwd`',g' yi-inplace.in > yi-inplace
	@chmod 755 yi-inplace

EXTRA_CLEANS+= yi-inplace

# Dependency orders

include $(TOPDIR)/depend

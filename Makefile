#
# hemacs Makefile
#

TOPDIR = .

include $(TOPDIR)/mk/config.mk

ALL_DIRS=       HEmacs cbits

BIN=            hemacs
STATIC_BIN=     hemacs-static
HS_BINS=        $(BIN) $(STATIC_BIN)

# Library specific stuff
 
PKG=            hemacs

# dynamic front end
 
BIN_OBJS=       Boot.o
BIN_DEPS=       plugins posix
BIN_LIBS=       $(CURSES) iconv

# static front end
 
STATIC_OBJS=    Main.o
STATIC_BIN_DEPS=hemacs posix
STATIC_BIN_LIBS=$(CURSES) iconv
STATIC_HC_OPTS  += -package-conf hemacs.conf

SHARED_OBJ=	HEmacs/BootAPI.o

#
# read in suffix rules
#
include $(TOPDIR)/mk/rules.mk

#
# Special targets
# 

#
# This module is shared between the static and dynamic code. It needs to be
# treated with care. In particular, it has to get archived into
# HShemacs.o, but it cannot have a -package hemacs flag.
#
HEmacs/BootAPI.o: HEmacs/BootAPI.hs
	$(GHC) $(HC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way)hi

#
# Boot is the bootstrap loader. It cant be linked *statically* against
# -package hemacs. It depends on $(SHARED_OBJ), which lives in HEmacs,
# hence the -iHEmacs flag.
#
Boot.o: Boot.hs 
	$(GHC) $(HC_OPTS) $(BIN_HC_OPTS) -DLIBDIR=\"$(LIBDIR)\" -iHEmacs -main-is Boot.main -c $< -o $@ -ohi $(basename $@).$(way)hi

#
# Main is the actual application Main.main, as well as being the frontend of
# the statically linked binary
#
# semi-magic to defeat <= ghc-6.2.1 use of -i. by default. this stops
# us using a library and it's .o files easily in the same dir -- the
# .o files will always be used over the package dependency. Not an
# issue in ghc-6.2.2. Anyway, the Solution: cd somewhere where -i. means nothing.
#
MAGIC_FLAGS   += -package-conf ../hemacs.conf  -package hemacs

Main.o: Main.hs
	( cd HEmacs ; $(GHC) $(HC_OPTS) $(MAGIC_FLAGS) -odir .. -c ../$< -o ../$@ -ohi ../$(basename $@).$(way)hi )

hemacs-inplace: hemacs-inplace.in
	sed 's,@HEMACS_TOP@,'`pwd`',g' hemacs-inplace.in > hemacs-inplace
	chmod 755 hemacs-inplace

EXTRA_CLEANS+= hemacs-inplace

# Dependency orders

include $(TOPDIR)/depend

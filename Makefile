#
# hemacs Makefile
#

TOPDIR = .

include $(TOPDIR)/system.mk

PACKAGE  	= hemacs

EXTRA_TARGET 	= hemacs
TARGET   	= hemacs-static
GHCI_LIB 	= HS$(PACKAGE).o
TARGET_A 	= libHS$(PACKAGE).a

GHC_COMPILE_FLAGS += -package-name $(PACKAGE)

# not part of HShemacs.o, but need to be dynamically loaded
EXTRA_OBJS = Main.o
EXTRA_HI_FILES = $(subst .o,.hi, $(EXTRA_OBJS))
BOOT_OBJS  = HEmacs/BootAPI.o Boot.o 

C_SOURCES   = cbits/nomacro.c

HSC_C_FILES = HEmacs/CWString_hsc.c HEmacs/Curses_hsc.c

RAW_SRCS = Locale.hsc GenUtil.hs CWString.hsc Curses.hsc IConv.hsc  \
           Version.hs Entry.hs Editor.hs MBox.hs Style.hs UI.hs \
	   BootAPI.hs ConfigAPI.hs MkTemp.hs

HS_SOURCES = $(patsubst %, HEmacs/%, $(RAW_SRCS))

TO_CLEAN += hemacs-static hemacs hemacs-inplace
TO_CLEAN += $(BOOT_OBJS) $(EXTRA_OBJS) Config.o Config.hi Boot.hi Main.hi
TO_CLEAN += $(TARGET_A) *.conf.install* *.conf *.old *.conf.in $(GHCI_LIB) *.bak

HEMACS_PACKAGE   = -package-conf hemacs.conf  -package hemacs
PLUGINS_PACKAGE  = -package plugins
HSC_INCLUDES    += -Icbits

LIBS               = $(LIBS_ICONV) $(EXTRALIBS)

GHC_LINK_FLAGS    += -l$(CURSES) $(LIBS) $(HEMACS_PACKAGE)
BOOT_LINK_FLAGS    = -l$(CURSES) $(LIBS) $(PLUGINS_PACKAGE)

include $(TOPDIR)/rules.mk

# statically linked target
$(TARGET): $(OBJS) $(EXTRA_OBJS)
	$(GHC) $(GHC_LINK_FLAGS) -o $@ $(EXTRA_OBJS)

#
# semi-magic to defeat <= ghc-6.2.1 use of -i. by default. this stops
# us using a library and it's .o files easily in the same dir -- the
# .o files will always be used over the package dependency. Not an
# issue in ghc-6.2.2. Anyway, the Solution: cd somewhere where -i. means nothing.
#
MAGIC_FLAGS   += -package-conf ../hemacs.conf  -package hemacs

Main.o: package_conf Main.hs
	( mkdir d ; cd d ;\
	  $(GHC) $(GHC_OPT_FLAGS) $(MAGIC_FLAGS) -odir .. -c ../Main.hs ;\
	  cd .. ; rmdir d)

# dynamic loader target
$(EXTRA_TARGET): package_conf hemacs-inplace $(BOOT_OBJS)
	$(GHC) $(BOOT_LINK_FLAGS) -o $@ $(BOOT_OBJS)

# -main-is Boot.main
Boot.o: Boot.hs
	$(GHC) -DLIBDIR=\"$(LIBDIR)\" $(GHC_WARNINGS) $(GHC_OPT_FLAGS) $(PLUGINS_PACKAGE) -main-is Boot.main -c Boot.hs

hemacs-inplace: hemacs-inplace.in
	sed 's,@HEMACS_TOP@,'`pwd`',g' hemacs-inplace.in > hemacs-inplace
	chmod 755 hemacs-inplace

install:
	@if [ ! -x $(TARGET) ]; then echo "Try 'make' first" ; fi
	$(INSTALLDIR) $(BINDIR)
	$(INSTALL) -m $(BIN_MODE) $(TARGET)       $(BINDIR)
	$(INSTALL) -m $(BIN_MODE) $(EXTRA_TARGET) $(BINDIR)
	$(INSTALLDIR) $(LIBDIR)/$(PACKAGE)/imports
	$(INSTALL) -m $(LIB_MODE) $(TARGET_A) $(LIBDIR)/$(PACKAGE)
	$(INSTALL) -m $(LIB_MODE) $(GHCI_LIB) $(LIBDIR)/$(PACKAGE)
	for i in $(HI_FILES) ; do \
		$(INSTALL) -m $(LIB_MODE) $$i $(LIBDIR)/$(PACKAGE)/imports ;\
	done
	for i in $(EXTRA_OBJS) $(EXTRA_HI_FILES) ; do \
		$(INSTALL) -m $(LIB_MODE) $$i $(LIBDIR)/$(PACKAGE) ;\
	done
	$(INSTALL) -m $(LIB_MODE) $(PACKAGE).conf.install $(LIBDIR)/$(PACKAGE)/$(PACKAGE).conf

# pull in generated deps
include $(TOPDIR)/depend

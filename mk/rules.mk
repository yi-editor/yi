#
# rules.mk : set flags and make rules
#

#
# pull in the file search stuff
#
include $(TOPDIR)/mk/paths.mk

PRE_SRCS:=      $(ALL_SRCS)

HC_OPTS        += $(DEFINES) -Icbits
HSC_OPTS       += $(DEFINES) -Icbits
CC_OPTS        += -Icbits

#
# Binary flags
#
BIN_HC_OPTS     += $(patsubst %,-package %, $(BIN_DEPS))
STATIC_HC_OPTS  += $(patsubst %,-package %, $(STATIC_BIN_DEPS))

BIN_LD_OPTS	= $(patsubst %,-l%, $(BIN_LIBS))
STATIC_LD_OPTS  = $(patsubst %,-l%, $(STATIC_BIN_LIBS))

#
# Library flags
#
PKG_OPTS       += -package-name $(PKG)
LIBOBJS		= $(filter-out $(BIN_OBJS) $(STATIC_BIN_OBJS), $(OBJS))
LIBRARY         = libHS$(PKG)$(_way).a
GHCI_LIBRARY    = $(patsubst lib%.a,%.o,$(LIBRARY))

#
# Compute dependencies
#
depend: $(MKDEPENDHS_SRCS)
	$(GHC) -M -optdep-f -optdepdepend $(HC_OPTS) $(MKDEPENDHS_SRCS)

#
#  boot and all targets
#
.PHONY: boot all

boot :: depend

all :: $(HS_BINS)

$(BIN) :: $(SHARED_OBJ) $(BIN_OBJS) hemacs-inplace
	$(GHC) -o $@ $(LD_OPTS) $(BIN_LD_OPTS) $(BIN_HC_OPTS) $(BIN_OBJS) $(SHARED_OBJ)

$(STATIC_BIN) :: $(LIBRARY) package $(STATIC_OBJS)
	$(GHC) -o $@ $(LD_OPTS) $(STATIC_LD_OPTS) $(STATIC_HC_OPTS) $(STATIC_OBJS)

EXTRA_CLEANS+= $(BIN) $(STATIC_BIN)

#
# Building libraries from $(LIBOBJS)
#
all :: $(LIBRARY)

$(LIBRARY) :: $(LIBOBJS)
	$(RM) $@
	$(AR) $(AR_OPTS) $@ $(LIBOBJS)
	$(RANLIB) $@

all :: $(GHCI_LIBRARY)

$(GHCI_LIBRARY) : $(LIBOBJS)
	$(LD) -r $(LD_X) -o $@ $(STUBOBJS) $(LIBOBJS)

# No need to define .SUFFIXES because we don't use any suffix rules
# Instead we use gmake's pattern rules exlusively

.SUFFIXES:

# This declaration tells GNU make to delete the target if it has
# changed and the command which created it exited with a non-zero exit
# code.

.DELETE_ON_ERROR:

#
# We anticipate wanting to use multiple ways. Particularly prof.
#

%.$(way)o: %.hs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@ -ohi $(basename $@).$(way)hi

%.$(way)o : %.lhs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way)hi

%.$(way)hi : %.$(way)o
	@:

%_hsc.c %_hsc.h %.hs : %.hsc
	$(HSC2HS) $(HSC_OPTS) $<
	@touch $(patsubst %.hsc,%_hsc.c,$<)

%.$(way)o : %.c
	@$(RM) $@
	$(GHC) $(CC_OPTS) -c $< -o $@

#
# Package creation
#
.PHONY: package
package:: $(PKG).conf $(PKG).conf.install

# in-tree package.conf
$(PKG).conf: $(PKG).conf.in.cpp
	cpp < $(PKG).conf.in.cpp | sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PKG).conf.in
	if [ ! -f $(PKG).conf ]; then echo [] > $(PKG).conf ; fi
	env PREFIX=`pwd`/$(TOPDIR) $(GHC_PKG) $(GHC_PKG_DEFINES) --force -f $(PKG).conf -u < $(PKG).conf.in

# installable package.conf
$(PKG).conf.install: $(PKG).conf.in.cpp
	cpp -DINSTALLING < $(PKG).conf.in.cpp | sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PKG).conf.install.in
	if [ ! -f $(PKG).conf.install ]; then echo [] > $(PKG).conf.install ; fi
	env PREFIX=$(PREFIX) $(GHC_PKG) $(GHC_PKG_DEFINES) --force -f $(PKG).conf.install -u < $(PKG).conf.install.in

EXTRA_CLEANS+= $(PKG).conf.install $(PKG).conf $(PKG).conf.in $(PKG).conf.install.in *.old

#
# cleaning
#
.PHONY: clean distclean

clean:
	$(RM) $(MOSTLY_CLEAN_FILES) $(EXTRA_CLEANS) $(CLEAN_FILES)

distclean: clean
	$(RM) $(DIST_CLEAN_FILES)

#
# installing
#
# For each of these variables that is defined, you
# get one install rule
#
#       INSTALL_PROGS        executable programs in $(bindir)
#       INSTALL_LIBS         platform-dependent libraries in $(libdir) (ranlib'ed)
#       INSTALL_DATAS        platform-independent files in $(datadir)
#       INSTALL_IFACES       platform-dependent interface files in $(ifacedir)
#

.PHONY: install install-dirs

INSTALL_PROGS  += $(HS_BINS)
INSTALL_IFACES += $(HS_IFACES) $(STATIC_IFACES)
INSTALL_LIBS   += $(LIBRARY) $(GHCI_LIBRARY) $(STATIC_OBJS) $(STATIC_IFACES)

show-install :
	@echo "BINDIR = $(BINDIR)"
	@echo "LIBDIR = $(LIBDIR)"

ifneq "$(INSTALL_PROGS)" ""
install :: $(INSTALL_PROGS)
	@$(INSTALL_DIR) $(BINDIR)
	@for i in $(INSTALL_PROGS); do \
		echo $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(BINDIR) ;\
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(BINDIR) ;\
	done
endif

ifneq "$(INSTALL_LIBS)" ""
install:: $(INSTALL_LIBS)
	@$(INSTALL_DIR) $(LIBDIR)
	@for i in $(INSTALL_LIBS); do \
		echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(LIBDIR) ;\
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(LIBDIR) ;\
	done
endif

ifneq "$(INSTALL_DATAS)" ""
install:: $(INSTALL_DATAS)
	@$(INSTALL_DIR) $(DATADIR)
	for i in $(INSTALL_LIBS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DATADIR) ;\
	done
endif

ifneq "$(INSTALL_IFACES)" ""
install:: $(INSTALL_IFACES)
	@$(INSTALL_DIR) $(IFACEDIR)
	for i in $(INSTALL_IFACES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(IFACEDIR)/`dirname $$i`; \
	done
endif

install:: $(PKG).conf.install
	@$(INSTALL_DIR) $(LIBDIR)
	$(INSTALL_DATA) $(INSTALL_OPTS) $< $(LIBDIR)/$(PKG).conf


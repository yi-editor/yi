#
# rules.mk : set flags and make rules
#

#
# pull in the file search stuff
#
include $(TOPDIR)/mk/paths.mk

PRE_SRCS:=      $(ALL_SRCS)

HC_OPTS        += $(DEFINES) -Icbits $(INC_OPTS)
HSC_OPTS       += $(DEFINES) -Icbits $(INC_OPTS)
CC_OPTS        += -Icbits -optc-O3 $(INC_OPTS)

# If $(way) is set then we define $(way_) and $(_way) from it in the
# obvious fashion.
ifeq "$(way)" "p"
  way_ := $(way)_
  _way := _$(way)
endif

#
# building the profiled way
#
ifeq "$(way)" "p"
PROF_OPTS	= -prof -auto-all
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS         += $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
endif

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

# hack. we don't want to have _yi-inplace..
$(BIN) :: $(BIN_OBJS) yi-inplace
	$(GHC) -o $@ $(LD_OPTS) $(BIN_LD_OPTS) $(BIN_HC_OPTS) $(BIN_OBJS)

$(STATIC_BIN) :: $(LIBRARY) $(PKG).conf $(PKG).conf.install $(LIB_FRONTEND) $(STATIC_OBJS)
	$(GHC) -o $@ $(LD_OPTS) $(STATIC_LD_OPTS) $(STATIC_HC_OPTS) $(STATIC_OBJS) $(LIB_FRONTEND)

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

%.$(way_)o: %.hs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi

%.$(way_)o : %.lhs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

%.$(way_)hi : %.$(way_)o
	@:

%_hsc.c %_hsc.h %.hs : %.hsc
	$(HSC2HS) $(HSC_OPTS) $<
	@touch $(patsubst %.hsc,%_hsc.c,$<)

%.$(way_)o : %.c
	@$(RM) $@
	$(GHC) $(CC_OPTS) -c $< -o $@

# preprocssed files, for haddock docs
%.raw-hs : %.lhs
	$(GHC) $(HC_OPTS) $(DEFINES) -D__HADDOCK__ -E -optP-P $< -o $@

%.raw-hs : %.hs
	$(GHC) $(HC_OPTS) $(DEFINES) -D__HADDOCK__ -E -optP-P $< -o $@

# Alex Suffix Rules
%.hs : %.x
	$(ALEX) $(ALEX_OPTS) $<

#
# Package creation
#

# in-tree package.conf
$(PKG).conf: $(PKG).conf.in.cpp
	cpp < $(PKG).conf.in.cpp | sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PKG).conf.in
	if [ ! -f $(PKG).conf ]; then echo [] > $(PKG).conf ; fi
	env PREFIX=`pwd` CURSES=$(CURSES) ICONV=$(ICONV) $(GHC_PKG) --force -f $(PKG).conf -u < $(PKG).conf.in

# installable package.conf
$(PKG).conf.install: $(PKG).conf.in.cpp
	cpp -DINSTALLING < $(PKG).conf.in.cpp | sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PKG).conf.install.in
	if [ ! -f $(PKG).conf.install ]; then echo [] > $(PKG).conf.install ; fi
	env PREFIX=$(PREFIX) CURSES=$(CURSES) ICONV=$(ICONV) $(GHC_PKG) --force -f $(PKG).conf.install -u < $(PKG).conf.install.in

EXTRA_CLEANS+= $(PKG).conf.install $(PKG).conf $(PKG).conf.in $(PKG).conf.install.in *.old

#
# Building the haddock docs
#
ifneq "$(HADDOCK)" ""

.PHONY: doc
docs :: html

HTML_DIR      = html
HADDOCK_SRCS += $(HS_SRCS)
HS_PPS        = $(addsuffix .raw-hs, \
                        $(filter-out $(basename $(NO_DOCS)), \
                                $(basename $(HADDOCK_SRCS))))

INSTALL_DATAS  += $(HTML_DIR)

html : $(HS_PPS)
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) $(HADDOCK_OPTS) -h -o $(HTML_DIR) $(HS_PPS) --package=$(PKG)

CLEAN_FILES += $(HS_PPS) $(PACKAGE).haddock

distclean ::
	$(RM) -rf $(HTML_DIR)

endif

#
# cleaning
#
.PHONY: clean distclean

clean:
	$(RM) $(MOSTLY_CLEAN_FILES) $(EXTRA_CLEANS) $(CLEAN_FILES)

distclean :: clean
	$(RM) $(DIST_CLEAN_FILES) *~ */*~

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
INSTALL_IFACES += $(HS_IFACES)
INSTALL_LIBS   += $(LIBRARY) $(GHCI_LIBRARY) $(LIB_FRONTEND) $(LIB_IFACE)

show-install :
	@echo "BINDIR  = $(BINDIR)"
	@echo "LIBDIR  = $(LIBDIR)"
	@echo "DATADIR = $(DATADIR)"

# the sed is to strip any trailing '_' from the inplace bin names.
ifneq "$(INSTALL_PROGS)" ""
install :: $(INSTALL_PROGS)
	@$(INSTALL_DIR) $(BINDIR)
	@for i in $(INSTALL_PROGS); do \
        j=`echo $$i | sed 's/_$$//'` ;\
		echo $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(BINDIR)/$$j ;\
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(BINDIR)/$$j ;\
	done
endif

ifneq "$(INSTALL_LIBS)" ""
install :: $(INSTALL_LIBS)
	@$(INSTALL_DIR) $(LIBDIR)
	@for i in $(INSTALL_LIBS); do \
		echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(LIBDIR) ;\
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(LIBDIR) ;\
	done
endif

ifneq "$(INSTALL_DATA)" ""
install :: $(INSTALL_DATAS)
	@$(INSTALL_DIR) $(DATADIR)
	@for i in $(INSTALL_DATAS); do \
		if [ -d $$i ] ; then \
			echo $(CP) -r $$i $(DATADIR)/ ;\
			$(CP) -r $$i $(DATADIR)/ ;\
		else \
			echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DATADIR)/ ;\
			$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(DATADIR)/ ;\
		fi ;\
	done
endif

ifneq "$(INSTALL_IFACES)" ""
install :: $(INSTALL_IFACES)
	@$(INSTALL_DIR) $(IFACEDIR)
	for i in $(INSTALL_IFACES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(IFACEDIR)/`dirname $$i`/ ; \
	done
endif

install :: $(PKG).conf.install
	@$(INSTALL_DIR) $(LIBDIR)
	$(INSTALL_DATA) $(INSTALL_OPTS) $< $(LIBDIR)/$(PKG).conf


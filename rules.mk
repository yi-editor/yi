
HSC_SOURCES = $(filter %.hsc, $(HS_SOURCES))

HSC_HS_FILES = $(subst .hsc,.hs, $(HSC_SOURCES))
HSC_C_INCLUDES = $(subst .c,.h, $(HSC_C_FILES))

HS_OBJS = $(subst .hs,.o, $(subst .hsc,.o, $(HS_SOURCES)))
HI_FILES = $(subst .hs,.hi, $(subst .hsc,.hi, $(HS_SOURCES)))

C_OBJS = $(subst .c,.o, $(C_SOURCES))
HSC_C_OBJS = $(subst .c,.o, $(HSC_C_FILES))

OBJS = $(C_OBJS) $(HSC_C_OBJS) $(HS_OBJS)

TO_CLEAN += $(OBJS) $(TARGET) $(HSC_C_INCLUDES) $(HI_FILES) $(TARGET_A)

# the real rules

.PHONY: all boot clean distclean

all: boot $(TARGET) $(TARGET_A) package_conf $(EXTRA_TARGET)

boot: $(HSC_HS_FILES) depend

clean:
	$(RM) $(TO_CLEAN)

distclean: clean
	find . -name '*~' -exec $(RM) {} \;
	$(RM) $(HSC_HS_FILES) $(HSC_C_FILES)

depend: $(HSC_HS_FILES) $(HS_SOURCES)                        
	$(GHC) -M -optdep-f -optdepdepend $(GHC_COMPILE_FLAGS) $(HSC_HS_FILES) $(HS_SOURCES)

# Generic rules:
#
.SUFFIXES : .o .hs .hi .lhs .hc .s

%.o: %.hs
	$(GHC) $(GHC_COMPILE_FLAGS) -c $< -o $@

%.o: %.c
	$(GHC) $(GHC_C_COMPILE_FLAGS) -c $< -o $@

.PRECIOUS: $(HSC_HS_FILES)
%.hs: %.hsc
	$(HSC2HS) $(HSC_INCLUDES) $< -o $@

%.hi: %.o
	@:

%_hsc.c: %.hs
	@:

%_hsc.h: %.hs
	@:

ifdef TARGET_A

$(TARGET_A): $(OBJS)
	$(AR) $(ARFLAGS) $@ $+
	ranlib $@

endif

ifdef PACKAGE

.PHONY: package_conf

package_conf : $(TARGET_A) $(PACKAGE).conf $(PACKAGE).conf.install
 
# in-tree package.conf
$(PACKAGE).conf: $(PACKAGE).conf.in.cpp
	rm -f $(GHCI_LIB)
	cpp < $(PACKAGE).conf.in.cpp | sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PACKAGE).conf.in
	if [ ! -f $(PACKAGE).conf ]; then echo [] > $(PACKAGE).conf ; fi
	env PREFIX=`pwd`/$(TOPDIR) $(GHC_PKG) $(GHC_PKG_DEFINES) \
		--force -g -f $(PACKAGE).conf -u < $(PACKAGE).conf.in

# installable package.conf
$(PACKAGE).conf.install: $(PACKAGE).conf.in.cpp
	cpp -DINSTALLING < $(PACKAGE).conf.in.cpp |\
		sed 's/""//g;s/\[ *,/[ /g;/^#/d' > $(PACKAGE).conf.install.in
	if [ ! -f $(PACKAGE).conf.install ]; then echo [] > $(PACKAGE).conf.install ; fi
	env PREFIX=$(PREFIX) $(GHC_PKG) $(GHC_PKG_DEFINES) \
		--force -f $(PACKAGE).conf.install -u < $(PACKAGE).conf.install.in
 
endif

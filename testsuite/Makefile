TOPDIR = ..

include $(TOPDIR)/mk/config.mk

PKG_CONF=       -package-conf $(TOPDIR)/yi.conf
PKG=            -package yi
BIN=            run-utests

M4=             /usr/local/bin/gm4
PP=             $(TOPDIR)/testsuite/pp/logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) $(PP_OPTS) -fglasgow-exts --make $(PKG_CONF) $(PKG)

all: run-utests

# build executable, linking in all tests
run-utests: 
	$(GHC_) -o $@ UnitTestsMain.hs

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) */*.o */*.hi */*~

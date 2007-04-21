cabal-make = .

build-args = --with-ghc=$(ghc-path)

include config.mk

include $(cabal-make)/cabal-make.inc

runtime-config:
	mkdir -p $(HOME)/.yi
	cp examples/*.hs $(HOME)/.yi

emacs: $(flavour) install
	$(prefix)/bin/yi --as=emacs

vim: $(flavour) install
	dist/build/yi/yi --as=vim

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

interactive:
	ghci -fglasgow-exts -package ghc -cpp -hidirdist/build/yi/yi-tmp/ -odirdist/build/yi/yi-tmp/ -i/home/jp/.yi ./dist/build/yi/yi-tmp/cbits/YiUtils.o Yi/Yi.hs 


yi-vty:
	make -C packages/yi-vty install
	-ghc-pkg --user hide yi-gtk

yi-gtk:
	make -C packages/yi-gtk install
	-ghc-pkg --user hide yi-vty

Contributors: Contributors.hs
	ghc --make $<

CONTRIBUTORS: Contributors _darcs/patches/*
	darcs changes | ./Contributors > $@

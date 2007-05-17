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


dists: 
	make sdist
	make -C packages/yi-vty sdist
	make -C packages/yi-gtk sdist
	rm -fr hackage
	mkdir -p hackage
	cp dist/yi-$(version).tar.gz hackage
	cp packages/yi-vty/dist/yi-vty-$(version).tar.gz hackage
	cp packages/yi-gtk/dist/yi-gtk-$(version).tar.gz hackage

test-dists:
	-ghc-pkg --user unregister yi-gtk
	-ghc-pkg --user unregister yi-vty

	cd hackage;\
	tar -zxvf yi-$(version).tar.gz;\
	tar -zxvf yi-vty-$(version).tar.gz;\
	tar -zxvf yi-gtk-$(version).tar.gz;\
	cd yi-$(version);\
	runghc Setup.hs configure --user --prefix=$(prefix);\
	runghc Setup.hs build --with-ghc=/usr/bin/ghc;\
	runghc Setup.hs install;\
	cd ..;\
	cd yi-vty-$(version);\
	runghc Setup.hs configure --user --prefix=$(prefix);\
	runghc Setup.hs build;\
	runghc Setup.hs install;\
	cd ..;\
	cd yi-gtk-$(version);\
	runghc Setup.hs configure --user --prefix=$(prefix);\
	runghc Setup.hs build;\
	runghc Setup.hs install;\
	cd ..;\

test-gtk:
	-ghc-pkg --user expose yi-gtk
	-ghc-pkg --user hide yi-vty
	$(prefix)/bin/yi


test-vty:
	-ghc-pkg --user expose yi-vty
	-ghc-pkg --user hide yi-gtk
	$(prefix)/bin/yi



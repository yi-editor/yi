cabal-make = .

include config.mk

include $(cabal-make)/cabal-make.inc

run-inplace: yi-lib install
	$(prefix)/bin/yi -B. -f$(flavour)

runtime-config:
	mkdir -p $(HOME)/.yi
	cp examples/*.hs $(HOME)/.yi

emacs: install yi-lib
	$(prefix)/bin/yi --as=emacs

vim: install yi-lib
	$(prefix)/bin/yi --as=vim

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

yi-lib:
	make -C packages/yi-lib install

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
	$(prefix)/bin/yi -fvty


test-vty:
	$(prefix)/bin/yi -fgtk



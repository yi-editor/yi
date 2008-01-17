cabal-make = .

include config.mk

include $(cabal-make)/cabal-make.inc

runtime-config:
	mkdir -p $(HOME)/.yi
	cp examples/*.hs $(HOME)/.yi

run-inplace: build #using YiConfig's keybinding.
	cp --preserve=timestamps -R dist/build/Yi/Syntax/*.hs Yi/Syntax
# the above is the dirtiest hack to be able to run Yi "in-place".
	dist/build/yi/yi -B. -bdist/build/ -f$(frontend)

emacs: build
	dist/build/yi/yi -B. -bdist/build/ -f$(frontend) --as=emacs

vim: build
	dist/build/yi/yi -B. -bdist/build/ -f$(frontend) --as=vim

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

Contributors: Contributors.hs
	ghc --make $<

CONTRIBUTORS: Contributors _darcs/inventory
	darcs changes | ./Contributors > $@

activity.png: _darcs/inventory
	darcs-graph . --name=yi -o $@

dist/yi-$(version).tar.gz:
	make sdist # does not work atm 

test_prefix := $(shell pwd)/hackage

test-dist: dist/yi-$(version).tar.gz
	rm -fr hackage
	mkdir -p hackage
	cp dist/yi-$(version).tar.gz hackage
	cd hackage &&\
	tar zxvf yi-$(version).tar.gz &&\
	cd yi-$(version) &&\
	runghc Setup.hs configure --user --prefix=$(test_prefix) &&\
	runghc Setup.hs build &&\
	runghc Setup.hs install &&\
	cd ..;\


test-gtk:
	$(test_prefix)/bin/yi -fvty


test-vty:
	$(test_prefix)/bin/yi -fgtk



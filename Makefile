cabal-make = .

include config.mk

include $(cabal-make)/cabal-make.inc

test: build # should also run the test suite.

%.ps: %.hp
	hp2ps -c $<

prof-config::
	cabal configure -fhacking -f-gtk -f-pango --enable-executable-profiling --ghc-options=-auto-all

run-inplace: build
	dist/build/yi/yi -f$(frontend)

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

Contributors: Contributors.hs
	ghc --make $<

CONTRIBUTORS: Contributors _darcs/inventory
	darcs changes | ./Contributors > $@

activity.png: _darcs/inventory
	darcs-graph . -y20 --name=yi -o $@

haddock.view: haddock
	firefox dist/doc/html/yi/index.html

haddock.upload:
	rsync -r dist/doc/html/yi/ $(user)@community.haskell.org:/srv/code/yi/doc

dist/yi-$(version).tar.gz::
	cabal sdist # does not work atm 

test_prefix := $(shell pwd)/hackage

test-dist: sdist
	rm -fr hackage
	mkdir -p hackage
	cp dist/yi-$(version).tar.gz hackage
	cd hackage &&\
	tar zxvf yi-$(version).tar.gz &&\
	cd yi-$(version) &&\
	# cabal haddock &&\
	cabal install &&\
 	cd ..;\


test-gtk:
	$(test_prefix)/bin/yi -fvty


test-vty:
	$(test_prefix)/bin/yi -fgtk

HS := $(shell find Yi Shim Data -type f -name '[^.]*.hs') Yi.hs Main.hs
tags: $(HS)
	@ echo [tags]
	@ echo '!_TAG_FILE_SORTED	0	~' > tags
	@ hasktags -a -c $(HS)
	@ hasktags -e $(HS)

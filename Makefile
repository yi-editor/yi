cabal-make = .

include config.mk

include $(cabal-make)/cabal-make.inc

interactive:
	ghci -cpp -XRank2Types -XFlexibleContexts -XGeneralizedNewtypeDeriving -XDeriveDataTypeable -IYi/Lexer -idist/build/autogen -idist/build/yi/yi-tmp HackerMain.hs
# autogen -> Paths_
# dist/build/yi/yi-tmp -> preprocessed lexers

derive:
	derive -a Yi/KillRing.hs
	derive -a Yi/Window.hs
	derive -a Yi/WindowSet.hs
	derive -a Yi/Buffer/Undo.hs
	derive -a Yi/Buffer/Implementation.hs

test::
	cabal configure -f-cocoa -ftesting --disable-library-profiling
	cabal build
	cabal copy
# for this test rule to be complete, we need to also:
#	check that examples/yi.hs can be recompiled;
# and finally 
#	justcompiledyi/yi --self-check
# must pass.
#	
# annoyingly, it's also difficult to find the correct set of options
# for configuration.
# maybe we need "dummy" UI for testing purposes only.

%.ps: %.hp
	hp2ps -c $<

%.pdf: %.ps
	ps2pdf $<

prof-config::
	cabal configure -fhacking -f-cocoa -f-gtk -f-pango --enable-executable-profiling --ghc-options=-auto-all

run-inplace: build
	dist/build/yi/yi -f$(frontend)

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

Contributors: Contributors.hs
	ghc --make $<

CONTRIBUTORS: Contributors _darcs/hashed_inventory
	darcs changes | ./Contributors > $@

activity.png: _darcs/hashed_inventory
	darcs-graph . -y20 --name=yi -o $@

dist/yi-$(version).tar.gz:: sdist

test_prefix := $(shell pwd)/hackage

test-dist: sdist
	rm -fr hackage
	mkdir -p hackage
	cp dist/yi-$(version).tar.gz hackage
	cd hackage &&\
	tar zxvf yi-$(version).tar.gz &&\
	cd yi-$(version) &&\
	cabal haddock &&\
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

cabal-make = .


interactive:
	ghci -package ghc -cpp -XRank2Types -XFlexibleContexts -XGeneralizedNewtypeDeriving -XDeriveDataTypeable -IYi/Lexer -idist/build/autogen -idist/build/yi/yi-tmp -hide-package mtl HackerMain.hs
# autogen -> Paths_
# dist/build/yi/yi-tmp -> preprocessed lexers

build:
	cabal configure -ftesting -fhacking
	cabal build

# Test the source tree.
test::
	cabal configure -ftesting --disable-library-profiling
	cabal build
	dist/build/yi/yi -fbatch --self-check 
	cabal copy
	cabal register
	dist/build/yi/yi -fbatch -y examples/yi.hs
# annoyingly, we do not know which interface will be compiled in, beside 'batch',
# which is not good to put as default in the examples/yi.hs.

%.ps: %.hp
	hp2ps -c $<

%.pdf: %.ps
	ps2pdf $<

dist-config::
	cabal configure

prof-config-hacking::
	cabal configure -fhacking -f-pango --enable-executable-profiling --ghc-options=-auto-all

prof-config::
	cabal configure -f-pango --enable-executable-profiling --enable-library-profiling --ghc-options=-auto-all

# Gtk2Hs must be configured with --enable-profiling (as an option to ./configure during installation)
prof-config-pango::
	cabal configure -fpango -f-vty --enable-executable-profiling --enable-library-profiling --ghc-options=-auto-all

run-inplace: build
	dist/build/yi/yi

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

Contributors: Contributors.hs
	ghc --make $<

CONTRIBUTORS: Contributors _darcs/hashed_inventory
	darcs changes | ./Contributors > $@

%-activity.png:
	darcs-graph . -y20 --name=yi -o $@ -f `date -d "now - $*" +"%Y%m%d"`

# A few ways to use the above rule...
3year-activity.png::
year-activity.png::
6month-activity.png::
2month-activity.png::



test_prefix := $(shell pwd)/hackage

# test the distribution.
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

HS := $(shell find src/library/Yi src/library/Shim src/library/Data -type f -name '[^.]*.hs') src/library/Yi.hs src/library/Main.hs
tags: $(HS)
	@ echo [tags]
	@ echo '!_TAG_FILE_SORTED	0	~' > tags
	@ hasktags -a -c $(HS)

TAGS: $(HS)
	@ echo [TAGS]
	@ hasktags -e $(HS)


DOCDIR=dist/doc/html/yi

$(DOCDIR)/yi.txt: $(HS)
	cabal haddock --hoogle


yi.txt: $(DOCDIR)/yi.txt
	cat $< | grep -v "^_" | grep -v ":::" > $@
# hoogle stumbles on leading underscores. (20090323)
# hoogle stumbles on ::: (20090323)


%.hoo: %.txt
	hoogle --convert=$<


actions: BufferM-actions EditorM-actions YiM-actions
	cat $+ > $@

%-actions: yi.hoo
	hoogle --data=$< ":: $* a" | grep $* > $@

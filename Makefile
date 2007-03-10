tmp-dir = /tmp
user = jpbernardy
cabal-make = .

prefix = $(HOME)/usr/

configure-dirs = --prefix=$(prefix) --datadir=--prefix=$(prefix)
hscolour-css = $(cabal-make)/hscolour.css

haddock-interfaces=\
  http://haskell.org/ghc/docs/latest/html/libraries/base,/home/jp/haskell/ghc/libraries/base/base.haddock \
  http://haskell.org/ghc/docs/latest/html/libraries/QuickCheck,/home/jp/haskell/ghc/libraries/QuickCheck/QuickCheck.haddock

top-src-dir =

extra-configure-args = --user --with-haddock=$(prefix)/bin/haddock

HsColour = $(prefix)/bin/HsColour

include $(cabal-make)/cabal-make.inc

runtime-config: $(HOME)/.yi/YiConfig.hs

$(HOME)/.yi/YiConfig.hs: YiConfig.example.hs
	mkdir -p $(HOME)/.yi
	cp $< $@

emacs: build runtime-config
	dist/build/yi/yi --as=emacs

vim: build runtime-config
	dist/build/yi/yi --as=vim

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

interactive:
	ghci -fglasgow-exts -package ghc -cpp -hidirdist/build/yi/yi-tmp/ -odirdist/build/yi/yi-tmp/ -i/home/jp/.yi ./dist/build/yi/yi-tmp/cbits/YiUtils.o Yi/Yi.hs 

build: .setup-config
	./setup build --with-ghc=/usr/bin/ghc


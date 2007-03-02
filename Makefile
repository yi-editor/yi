HS_FILES=$(shell find . -path "./_darcs" -prune -o -name "*.hs" -print)

all: TAGS build

runtime-config: $(HOME)/.yi/YiConfig.hs

$(HOME)/.yi/YiConfig.hs: YiConfig.example.hs
	mkdir -p $(HOME)/.yi
	cp $< $@

emacs: runtime-config
	dist/build/yi/yi --as=emacs

vim: runtime-config
	dist/build/yi/yi --as=vim

build:
	@runhaskell Setup.hs --with-ghc=ghc build

config:
	@runhaskell Setup.hs configure

html:
	@runhaskell Setup.hs haddock

install:
	@runhaskell Setup.hs install

clean:
	if [ -f .setup-config ]; then runhaskell Setup.hs clean; fi
	rm -f conftest*

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

tags TAGS: $(HS_FILES)
	hasktags -b $(HS_FILES)

interactive:
	ghci -fglasgow-exts -package ghc -cpp -hidirdist/build/yi/yi-tmp/ -odirdist/build/yi/yi-tmp/ -i/home/jp/.yi ./dist/build/yi/yi-tmp/cbits/YiUtils.o Yi/Yi.hs 
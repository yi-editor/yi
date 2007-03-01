HS_FILES=$(shell find . -path "./_darcs" -prune -o -name "*.hs" -print)

all: TAGS build

gtk:
	cd gtk && @runhaskell Setup.hs build

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
	ghci -fglasgow-exts -package ghc -cpp -hidirdist/build/yi/yi-tmp/ -odirdist/build/yi/yi-tmp/ ./dist/build/yi/yi-tmp/cbits/YiUtils.o Yi/Yi.hs
all:
	@runhaskell Setup.lhs build

config:
	@runhaskell Setup.lhs configure

html:
	@runhaskell Setup.lhs haddock

install:
	@runhaskell Setup.lhs install

clean:
	if [ -f .setup-config ]; then runhaskell Setup.lhs clean; fi
	rm -f conftest* Yi/Curses/*_hsc.[ch]

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

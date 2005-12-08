all:
	@runghc Setup.hs build

config:
	@runghc Setup.hs configure

install:
	@runghc Setup.hs install

clean:
	if [ -f .setup-config ]; then runghc Setup.hs clean; fi
	rm -f conftest* Yi/Curses/*_hsc.[ch]

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

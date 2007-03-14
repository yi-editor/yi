cabal-make = .


include config.mk

include $(cabal-make)/cabal-make.inc

runtime-config: $(HOME)/.yi/YiConfig.hs

$(HOME)/.yi/YiConfig.hs: YiConfig.example.hs
	mkdir -p $(HOME)/.yi
	cp $< $@

emacs: yi-vty install runtime-config
	$(prefix)/bin/yi --as=emacs

vim: yi-vty install runtime-config
	dist/build/yi/yi --as=vim

emacs-gtk: yi-gtk install runtime-config
	$(prefix)/bin/yi-gtk --as=emacs

distclean: clean
	rm -f yi.buildinfo testsuite/pp/logpp config.log config.cache config.status cbits/config.h .setup-config
	rm -rf dist

maintainer-clean: distclean
	rm -f configure cbits/config.h.in
	rm -rf autom4te.cache

interactive:
	ghci -fglasgow-exts -package ghc -cpp -hidirdist/build/yi/yi-tmp/ -odirdist/build/yi/yi-tmp/ -i/home/jp/.yi ./dist/build/yi/yi-tmp/cbits/YiUtils.o Yi/Yi.hs 


yi-vty:
	make -C packages/yi-vty install

yi-gtk:
	make -C packages/yi-gtk install
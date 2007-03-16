# Where to find ghc
build-args = --with-ghc=/usr/bin/ghc

# yi-vty or yi-gtk
flavour=yi-vty

# where to install executable and packages (running in-place -- without installing -- is not supported)
prefix = $(HOME)/usr/





# The following are web-publishing options.

tmp-dir = /tmp
user = jpbernardy

configure-dirs = --prefix=$(prefix) --datadir=--prefix=$(prefix)
hscolour-css = $(cabal-make)/hscolour.css

haddock-interfaces=\
  http://haskell.org/ghc/docs/latest/html/libraries/base,/home/jp/haskell/ghc/libraries/base/base.haddock \
  http://haskell.org/ghc/docs/latest/html/libraries/QuickCheck,/home/jp/haskell/ghc/libraries/QuickCheck/QuickCheck.haddock


extra-configure-args = --user --with-haddock=$(prefix)/bin/haddock

HsColour = $(prefix)/bin/HsColour



# You should never need to change the following.

top-src-dir =


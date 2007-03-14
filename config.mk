export ghc=

export tmp-dir = /tmp
export user = jpbernardy

export prefix = $(HOME)/usr/

export configure-dirs = --prefix=$(prefix) --datadir=--prefix=$(prefix)
export hscolour-css = $(cabal-make)/hscolour.css

export haddock-interfaces=\
  http://haskell.org/ghc/docs/latest/html/libraries/base,/home/jp/haskell/ghc/libraries/base/base.haddock \
  http://haskell.org/ghc/docs/latest/html/libraries/QuickCheck,/home/jp/haskell/ghc/libraries/QuickCheck/QuickCheck.haddock

export top-src-dir =

export extra-configure-args = --user --with-haddock=$(prefix)/bin/haddock

export HsColour = $(prefix)/bin/HsColour

export build-args = --with-ghc=/usr/bin/ghc

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yi";

  buildInputs = [
      cairo
      pango
      gtk
      haskell.compiler.ghc7103
      haskellPackages.gtk2hs-buildtools
      icu.out
      ncurses.out
      pkgconfig
      glibcLocales
  ];

  shellHook = ''
    export LC_ALL=en_US.UTF-8
    export LD_LIBRARY_PATH="${icu.out}/lib:${ncurses.out}/lib:$LD_LIBRARY_PATH"
  '';
}
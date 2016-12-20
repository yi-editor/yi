{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let ghc = haskell.packages.ghc801.ghcWithPackages (pkgs: [pkgs.gtk2hs-buildtools]);
in

stdenv.mkDerivation {
  name = "yi";

  buildInputs = [
      cairo
      pango
      gtk
      ghc
      haskellPackages.gtk2hs-buildtools
      icu.out
      ncurses.out
      pkgconfig
      glibcLocales
      zlib.out
  ];

  shellHook = ''
    export LC_ALL=en_US.UTF-8
    export PATH="${haskellPackages.gtk2hs-buildtools}/bin:$PATH"
    export CPATH="${icu.dev}/include:${ncurses.dev}/include:$CPATH"
    export LIBRARY_PATH="${icu.dev}/lib:${ncurses.dev}/lib:${zlib.out}/lib:$LIBRARY_PATH"
    export LD_LIBRARY_PATH="${icu.out}/lib:${ncurses.out}/lib:$LD_LIBRARY_PATH"
  '';
}

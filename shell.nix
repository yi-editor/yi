{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yi";

  buildInputs = [
      icu.out
      ncurses.out
      haskell.compiler.ghc7103
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${icu.out}/lib:${ncurses.out}/lib:$LD_LIBRARY_PATH"
  '';
}
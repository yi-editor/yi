{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yi";

  buildInputs = [
      icu.out
      haskell.compiler.ghc801
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${icu.out}/lib:$LD_LIBRARY_PATH"
  '';
}
let
  withPango = true;
  pkgs = import <nixpkgs> { };
  haskellPackages = pkgs.haskellPackages_ghc783;
in
rec {
  yi =

    haskellPackages.cabal.mkDerivation (self: {
      pname = "yi";
      version = "0.9.1";
      src = <yi> + "/yi";
      buildDepends = with haskellPackages; [
        # As imported above
        binary Cabal cautiousFile concreteTyperep dataDefault derive Diff
        dlist dyre filepath fingertree hashable hint lens mtl
        parsec pointedlist QuickCheck random regexBase regexTdfa safe
        split time transformersBase uniplate unixCompat unorderedContainers
        utf8String vty xdgBasedir tfRandom text cabalInstall
      ] ++ (if withPango then [ pango gtk glib ] else [ ]);
      buildTools = [ haskellPackages.alex ];
      testDepends = with haskellPackages; [ filepath HUnit QuickCheck tasty
                                            tastyHunit tastyQuickcheck ];

      postInstall = ''
        mv $out/bin/yi $out/bin/.yi-wrapped
        cat - > $out/bin/yi <<EOF
        #! ${self.stdenv.shell}
        # Trailing : is necessary for it to pick up Prelude &c.
        export GHC_PACKAGE_PATH=$(${self.ghc.GHCGetPackages} ${self.ghc.version} \
                                  | sed 's/-package-db\ //g' \
                                  | sed 's/^\ //g' \
                                  | sed 's/\ /:/g')\
        :$out/lib/ghc-${self.ghc.version}/package.conf.d/yi-$version.installedconf:

        eval exec $out/bin/.yi-wrapped "\$@"
        EOF
        chmod +x $out/bin/yi
      '';
      isLibrary = true;
      isExecutable = true;
      enableSplitObjs = false;
      doCheck = true;
      noHaddock = true;
      configureFlags = [ (if withPango then "-fpango" else "-f-pango") ];
    });

  yi-contrib =

    haskellPackages.cabal.mkDerivation (self: {
      pname = "yi-contrib";
      version = "0.9.1";
      src = <yi> + "/yi-contrib";
      buildDepends = with haskellPackages; [
        filepath lens mtl split time transformersBase yi
      ];
      meta = {
        homepage = "http://haskell.org/haskellwiki/Yi";
        description = "Add-ons to Yi, the Haskell-Scriptable Editor";
        license = "GPL";
        platforms = self.ghc.meta.platforms;
        maintainers = with self.stdenv.lib.maintainers; [ fuuzetsu ];
      };
    });

  yi-monokai =
    haskellPackages.cabal.mkDerivation (self: {
      pname = "yi-monokai";
      version = "0.1.1.2";
      src = <yi-monokai>;
      buildDepends = with haskellPackages; [ yi ];
      meta = {
        homepage = "https://github.com/Fuuzetsu/yi-monokai";
        description = "Monokai colour theme for the Yi text editor";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      };
    });

  # yi-haskell-utils =
  #   haskellPackages.cabal.mkDerivation (self: {
  #     pname = "yi-haskell-utils";
  #     version = "0.1.0.0";
  #     src = <yi-haskell-utils>;
  #     buildDepends = [
  #       dataDefault derive ghcMod lens network PastePipe split yi
  #     ];
  #     meta = {
  #       homepage = "https://github.com/Fuuzetsu/yi-haskell-utils";
  #       description = "Collection of functions for working with Haskell in Yi";
  #       license = self.stdenv.lib.licenses.gpl3;
  #       platforms = self.ghc.meta.platforms;
  #     };
  #   });

}

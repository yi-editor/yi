# To pin a new Haskell LTS in nixpkgs, get its commit sha (e.g. 7aa6d2c808b14aa04dc93c2ca64344c9eee0876a)
# and run:
# nix-prefetch-git \
#   --url https://github.com/NixOS/nixpkgs-channels \
#   --rev 7aa6d2c808b14aa04dc93c2ca64344c9eee0876a \
# > nixpkgs.json

# To build yi, run:
# nix build -f default.nix haskellPackages.yi

{ hostPkgs ? import <nixpkgs> {}
, pinnedVersion ? hostPkgs.lib.importJSON ./nixpkgs.json
}:

let
  pkgs = let
    pinnedPkgs = hostPkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      inherit (pinnedVersion) rev sha256;
    }; in import pinnedPkgs {};
in pkgs // rec {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: rec {
      yi                       = super.callCabal2nix "yi"                       ./yi {};
      yi-core                  = super.callCabal2nix "yi-core"                  ./yi-core {};
      yi-dynamic-configuration = super.callCabal2nix "yi-dynamic-configuration" ./yi-dynamic-configuration {};
      yi-frontend-pango        = super.callCabal2nix "yi-frontend-pango"        ./yi-frontend-pango {};
      yi-frontend-vty          = super.callCabal2nix "yi-frontend-vty"          ./yi-frontend-vty {};
      yi-fuzzy-open            = super.callCabal2nix "yi-fuzzy-open"            ./yi-fuzzy-open {};
      yi-intero                = super.callCabal2nix "yi-intero"                ./yi-intero {};
      yi-ireader               = super.callCabal2nix "yi-ireader"               ./yi-ireader {};
      yi-keymap-cua            = super.callCabal2nix "yi-keymap-cua"            ./yi-keymap-cua {};
      yi-keymap-emacs          = super.callCabal2nix "yi-keymap-emacs"          ./yi-keymap-emacs {};
      yi-keymap-vim            = super.callCabal2nix "yi-keymap-vim"            ./yi-keymap-vim {};
      yi-language              = super.callCabal2nix "yi-language"              ./yi-language {};
      yi-misc-modes            = super.callCabal2nix "yi-misc-modes"            ./yi-misc-modes {};
      yi-mode-haskell          = super.callCabal2nix "yi-mode-haskell"          ./yi-mode-haskell {};
      yi-mode-javascript       = super.callCabal2nix "yi-mode-javascript"       ./yi-mode-javascript {};
      yi-snippet               = super.callCabal2nix "yi-snippet"               ./yi-snippet {};
    };
  };
}

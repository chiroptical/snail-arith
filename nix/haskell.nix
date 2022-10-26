{pkgs, ...}:
let
  snail-shell = pkgs.callPackages ./snail-shell.nix {};
in
  pkgs.haskell.packages.ghc924.extend (final: prev: {
    "snail-shell" = final.callCabal2nix "snail-shell" snail-shell {};
  })

{ pkgs, snailSrc }:
pkgs.haskell.packages.ghc910.extend (
  final: prev: {
    snail = final.callCabal2nix "snail" snailSrc { };
    snail-arith = final.callCabal2nix "snail-arith" ./. { };
  }
)

{ pkgs, snailSrc }:
let
  haskell = pkgs.callPackage ./haskell.nix { inherit snailSrc; };
in
haskell.snail-arith

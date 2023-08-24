{
  pkgs,
  snail,
  ...
}: let
  haskell = pkgs.callPackage ./nix/haskell.nix {
    inherit snail;
  };
in
  haskell.callCabal2nix "snail-arith" ./. {}

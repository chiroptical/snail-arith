{
  pkgs,
  snail-shell,
  ...
}: let
  haskell = pkgs.callPackage ./nix/haskell.nix {
    inherit snail-shell;
  };
in
  haskell.callCabal2nix "snail-arith" ./. {}

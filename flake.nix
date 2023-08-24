{
  description = "snail-arith";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    snail-src.url = "github:chiroptical/snail";
  };

  outputs = {
    # self,
    nixpkgs,
    flake-utils,
    snail-src,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      snail = snail-src.packages.${system}.snail;
      snail-arith = import ./snail-arith.nix {
        inherit pkgs;
        inherit snail;
      };
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit snail;
      };
      defaultPackage = snail-arith;
      packages = flake-utils.lib.flattenTree {
        inherit snail-arith;
      };
    });
}

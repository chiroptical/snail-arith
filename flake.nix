{
  description = "snail-arith";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    snail-shell.url = "github:chiroptical/snail-shell";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    snail-shell,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      snail-arith = pkgs.callPackage ./snail-arith.nix {
        snail-shell = snail-shell.packages.${system}.snail-shell;
      };
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        snail-shell = snail-shell.packages.${system}.snail-shell;
      };
      defaultPackage = snail-arith;
      packages = flake-utils.lib.flattenTree {
        inherit snail-arith;
      };
    });
}

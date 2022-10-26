{
  description = "snail-arith";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    snail-shell-src.url = "github:chiroptical/snail-shell";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    snail-shell-src,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      snail-shell = snail-shell-src.packages.${system}.snail-shell;
      snail-arith = import ./snail-arith.nix {
        inherit pkgs;
        inherit snail-shell;
      };
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit snail-shell;
      };
      defaultPackage = snail-arith;
      packages = flake-utils.lib.flattenTree {
        inherit snail-arith;
      };
    });
}

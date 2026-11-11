{
  description = "The arith language from Types and Programming Languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  inputs.snail = {
    url = "github:chiroptical/snail/v0.1.2.2";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      snail,
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixFormatter =
        pkgs:
        pkgs.nixfmt-tree.override {
          settings.tree-root-file = "flake.nix";
        };
    in
    {
      devShells = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.callPackage ./shell.nix {
          nixFormatter = self.formatter.${system};
          snailSrc = snail;
        };
      });

      formatter = forAllSystems (system: nixFormatter nixpkgs.legacyPackages.${system});

      packages = forAllSystems (
        system:
        let
          snail-arith = import ./snail-arith.nix {
            pkgs = nixpkgs.legacyPackages.${system};
            snailSrc = snail;
          };
        in
        {
          inherit snail-arith;
          default = snail-arith;
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          test = pkgs.haskell.lib.doCheck self.packages.${system}.snail-arith;
          format =
            pkgs.runCommand "snail-arith-format"
              {
                nativeBuildInputs = [
                  pkgs.findutils
                  pkgs.gnumake
                  pkgs.haskellPackages.fourmolu
                  self.formatter.${system}
                ];
              }
              ''
                cp -R ${self} source
                chmod -R u+w source
                cd source
                make format-check
                touch "$out"
              '';
        }
      );
    };
}

{
  pkgs,
  snail-shell,
  ...
}:
pkgs.mkShell {
  inputsFrom = [
    (import ./snail-arith.nix {
      inherit pkgs;
      inherit snail-shell;
    })
    .env
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.fourmolu
    alejandra
  ];
  withHoogle = true;
  # required for 'make test' hedgehog output
  LANG = "en_US.utf8";
}

{
  pkgs,
  nixFormatter,
  snailSrc,
}:
let
  haskell = pkgs.callPackage ./haskell.nix { inherit snailSrc; };
in
haskell.shellFor {
  packages = p: [ p.snail-arith ];
  nativeBuildInputs = [
    haskell.cabal-install
    haskell.fourmolu
    haskell.ghcid
    haskell.haskell-language-server
    haskell.hlint
    haskell.hpack
    nixFormatter
  ];
  withHoogle = true;
}

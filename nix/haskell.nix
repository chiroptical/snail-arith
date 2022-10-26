{
  pkgs,
  snail-shell,
  ...
}:
pkgs.haskell.packages.ghc924.extend (final: prev: {
  "snail-shell" = snail-shell;
})

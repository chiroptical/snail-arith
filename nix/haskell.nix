{
  pkgs,
  snail,
  ...
}:
pkgs.haskell.packages.ghc94.extend (final: prev: {
  "snail" = snail;
})

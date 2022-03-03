let
  packages = import ./nix;
in {
  inherit (packages) pkgs auction plutus cardano-node;

  inherit (packages.auction.haskell) project;
}

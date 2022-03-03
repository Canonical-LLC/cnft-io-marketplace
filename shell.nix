let
  packages = import ./.;
  inherit (packages) pkgs auction cardano-node;
  inherit (auction) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with auction; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      cardano-node.cardano-cli
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }

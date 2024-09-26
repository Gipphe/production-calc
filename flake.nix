{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        package = {
          name = "production-calc";
          root = ./.;
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv (
              with pkgs;
              (
                [ nixfmt-rfc-style ]
                ++ (with haskellPackages; [
                  hlint
                  fourmolu
                  cabal-install
                  cabal-fmt
                  hpack
                ])
              )
            );
        };
      in
      {
        packages.default = pkgs.haskellPackages.developPackage package;
        devShells.default = pkgs.haskellPackages.developPackage (package // { returnShellEnv = true; });
      }
    );
}

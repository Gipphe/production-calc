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

        lint = pkgs.writeShellApplication {
          name = "lint";
          runtimeInputs = with pkgs; [
            haskellPackages.hlint
            yq
          ];
          text = ''
            exts=$(yq -r '.["default-extensions"] | .[]' package.yaml | xargs)
            prunedExts=('DataKinds')
            filteredExts=()
            for ext in $exts; do
              for pruneExt in "''${prunedExts[@]}"; do
                if test "$ext" = "$pruneExt"; then
                  continue
                fi
              done
            done
            extparam=()
            for ext in "''${filteredExts[@]}"; do
              extparam+=("-X $ext")
            done
            IFS=''' hlint "''${extparam[@]}" "$@"
          '';
        };
        package = {
          name = "production-calc";
          root = ./.;
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv (
              [ lint ]
              ++ (
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

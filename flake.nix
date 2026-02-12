{
  inputs.miso.url = "github:haskell-miso/miso";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.miso.inputs.nixpkgs) lib;
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" "LICENSE" ])
          root;
      };
      pname = "miso-graphql";
    in
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.miso.inputs.nixpkgs.legacyPackages.${system}.extend (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                miso = hfinal.callCabal2nix "miso" inputs.miso { };
                ${pname} = hfinal.callCabal2nix pname (sourceFilter ./.) { };
              })
            ];
          };
        });
      in
      {
        devShells = {
          default = pkgs.haskell.packages.ghc9122.shellFor {
            packages = ps: [ ps.${pname} ];
            nativeBuildInputs = [
              pkgs.haskellPackages.cabal-install
              pkgs.haskell.packages.ghc9122.haskell-language-server
            ];
          };
          wasm = inputs.miso.outputs.devShells.${system}.wasm;
          devShells.ghcjs = inputs.miso.outputs.devShells.${system}.ghcjs;
        };
      }
    );
}

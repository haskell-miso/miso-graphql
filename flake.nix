{
  inputs.miso.url = "github:haskell-miso/miso";

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShells = {
        default = inputs.miso.outputs.devShells.${system}.default;
        wasm = inputs.miso.outputs.devShells.${system}.wasm;
        devShells.ghcjs = inputs.miso.outputs.devShells.${system}.ghcjs;
      };
    });
}

{ inputs, ... }:
{
  perSystem = { lib, config, ... }:
    let
      overlays = [
        inputs.haskellNix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
        (import ./project.nix)
        (_: _: {
          inherit inputs;
          flake = config;
        })
      ];
      pkgs = config._module.args.pkgs.extend (lib.composeManyExtensions overlays);
      flake = pkgs.ytxp-sdk.flake { };
    in
    {
      packages = with flake.packages; {
        inherit "ytxp-sdk:lib:ytxp-sdk";
      };
      inherit (flake) devShells;
    };
}

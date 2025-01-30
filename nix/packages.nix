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
      flake-96 = pkgs.ytxp-sdk-96.flake { };
      flake-810 = pkgs.ytxp-sdk-810.flake { };
    in
    {
      packages = {
        ytxp-lib-96 = flake-96.packages."ytxp-sdk:lib:ytxp-sdk";
        ytxp-lib-810 = flake-810.packages."ytxp-sdk:lib:ytxp-sdk";
        ytxp-lib-96-test = flake-96.packages."ytxp-sdk:test:ytxp-sdk-test";
        ytxp-lib-810-test = flake-810.packages."ytxp-sdk:test:ytxp-sdk-test";
      };
      inherit (flake-96) devShells;
    };
}

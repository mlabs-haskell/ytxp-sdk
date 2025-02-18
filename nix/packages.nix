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
      combine-haddock = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/Plutonomicon/plutarch-plutus/f78b70eca8841a4a009cb6791a59c999cbc68745/nix/combine-haddock.nix";
        sha256 = "sha256-Th7HnBErgCdwwdszZ8gQz94V87gqEbzHAqN7QhRROMc=";
      };
    in
    {
      packages = {
        ytxp-lib-96 = flake-96.packages."ytxp-sdk:lib:ytxp-sdk";
        ytxp-lib-810 = flake-810.packages."ytxp-sdk:lib:ytxp-sdk";
        ytxp-lib-96-test = flake-96.packages."ytxp-sdk:test:ytxp-sdk-test";
        ytxp-lib-810-test = flake-810.packages."ytxp-sdk:test:ytxp-sdk-test";

        # Add documentation for ytxp-lib-96
        ytxp-lib-96-docs = import combine-haddock { inherit pkgs lib; } {
          cabalProject = pkgs.ytxp-sdk-96;
          targetPackages = [
            "ytxp-sdk"
          ];
          prologue = ''
            = YTxP SDK Documentation
            Documentation for the YTxP SDK
          '';
        };
        serve-docs-96 = pkgs.writeShellApplication {
          name = "serve-docs-96";
          text = "${lib.getExe pkgs.python3} -m http.server -d ${config.packages.ytxp-lib-96-docs}/share/doc 8284";
        };

        # Add documentation for ytxp-lib-810
        ytxp-lib-810-docs = import combine-haddock { inherit pkgs lib; } {
          cabalProject = pkgs.ytxp-sdk-810;
          targetPackages = [
            "ytxp-sdk"
          ];
          prologue = ''
            = YTxP SDK Documentation
            Documentation for the YTxP SDK
          '';
        };
        serve-docs-810 = pkgs.writeShellApplication {
          name = "serve-docs-810";
          text = "${lib.getExe pkgs.python3} -m http.server -d ${config.packages.ytxp-lib-810-docs}/share/doc 8284";
        };
      };
      inherit (flake-96) devShells;
    };
}

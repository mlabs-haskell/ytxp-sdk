{
  imports = [
    ./pre-commit.nix
  ];
    perSystem = { lib, config, pkgs, ... }: {
    checks = {
      # TODO add tests
      run-serialization-test = pkgs.runCommandNoCC "run-serialization-test" { } ''
        ${lib.getExe config.packages."ytxp-sdk:test:ytxp-sdk-test"} && touch $out
      '';
    };
  };
}

{
  imports = [
    ./pre-commit.nix
  ];
  perSystem = { ... }: {
    checks = {
      # TODO add tests
      # run-TEST_SUITE_NAME-test = pkgs.runCommandNoCC "run-TEST_SUITE_NAME-test" { } ''
      #   ${lib.getExe config.packages."ytxp-sdk:test:TEST_SUITE_NAME"} && touch $out
      # '';
    };
  };
}

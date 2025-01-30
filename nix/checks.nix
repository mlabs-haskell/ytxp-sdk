{
  imports = [
    ./pre-commit.nix
  ];
    perSystem = { lib, config, pkgs, ... }: {
    checks = {
      # TODO add tests
      run-serialization-test-810 = pkgs.runCommandNoCC "run-serialization-test-810" { } ''
        ${lib.getExe config.packages.ytxp-lib-810-test} && touch $out
	'';
      run-serialization-test-96 = pkgs.runCommandNoCC "run-serialization-test-96" { } ''
        ${lib.getExe config.packages.ytxp-lib-96-test} && touch $out
      '';	
    };
  };
}



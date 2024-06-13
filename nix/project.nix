final: _prev: {
  ytxp-sdk-96 = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc96";
    src = final.lib.cleanSource ./..;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
  };
  ytxp-sdk-810 = final.haskell-nix.cabalProject' {
    compiler-nix-name = "ghc810";
    src = final.lib.cleanSource ./..;
    shell = import ./shell.nix {
      flake = final.flake;
      pkgs = final;
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = final.inputs.CHaP;
    };
  };
}

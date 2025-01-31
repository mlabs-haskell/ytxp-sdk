{
  description = "Yielding Transaction Pattern SDK";
  inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    git-hooks.url = "github:cachix/git-hooks.nix";
    flake-root.url = "github:srid/flake-root";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.treefmt-nix.flakeModule
        ./nix/treefmt.nix
        ./nix/pre-commit.nix
        ./nix/packages.nix
        ./nix/checks.nix
      ];
    };
}

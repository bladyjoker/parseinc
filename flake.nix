{
  description = "ParseInc incremental parser combinator library";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    # flake-lang.nix for monorepo setup
    flake-lang.url = "github:mlabs-haskell/flake-lang.nix";

    # flake-parts for Flake modules
    flake-parts.follows = "flake-lang/flake-parts";

    # Hercules CI effects
    hci-effects.follows = "flake-lang/hci-effects";

    # Code quality automation
    pre-commit-hooks.follows = "flake-lang/pre-commit-hooks";

    # Cardano transaction library
    haskell-nix.follows = "flake-lang/haskell-nix";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix
        ./build.nix
      ];
      debug = true;
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    };

}

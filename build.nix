{ inputs, ... }:
{
  perSystem =
    {
      config,
      lib,
      pkgs,
      system,
      ...
    }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellFlake {
        src = ./.;

        name = "parseinc";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in

    {
      devShells.dev-parseinc = hsFlake.devShells.default;

      packages = {

        parseinc-src = pkgs.stdenv.mkDerivation {
          name = "parseinc-src";
          src = ./.;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      }
      // hsFlake.packages;

      inherit (hsFlake) checks;

    };
}

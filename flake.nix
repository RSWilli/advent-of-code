{
  description = "Advent of code";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.fenix.url = "github:nix-community/fenix";

  outputs = {
    self,
    flake-utils,
    devshell,
    nixpkgs,
    fenix,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;

        overlays = [
          devshell.overlays.default
          # fenix.overlays.default
        ];
      };
      setupday = pkgs.writeShellScriptBin "setupday" (builtins.readFile ./scripts/setupday.sh);
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.devshell.mkShell ({config, ...}: {
        name = "AOC devshell";
        # devshell.startup.pre-commit = self.checks.${system}.pre-commit-check.shellHook;
        packages = with pkgs; [
          # Rust build inputs
          pkg-config
          openssl

          # LSP's
          rust-analyzer

          # needed for linking and compiling rust
          gcc

          # rust
          (fenix.packages.${system}.stable.withComponents [
            "cargo"
            "clippy"
            "rust-src"
            "rustc"
            "rustfmt"
          ])

          # Haskell
          haskell.compiler.ghc98
          cabal-install
        ];
        commands = [
          {
            name = "setupday";
            help = "Setup a new day";
            package = setupday;
          }
        ];
      });
    });
}

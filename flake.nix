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
      setupday = pkgs.writeShellScriptBin "setupday" ''
        #!usr/bin/env bash

        DATE=""

        # get the current day of month from param or use today
        if [ -z "$1" ]; then
            # when using date, remove 0 padding
            DATE=$(date +%d | sed 's/^0*//')
        else
            DATE=$1
        fi

        # get the current year from param or use today
        if [ -z "$2" ]; then
            YEAR=$(date +%Y)
        else
            YEAR=$2
        fi

        PADDED_DATE=$(printf "%02d" $DATE)

        echo "Setting up day $DATE"

        touch "tests/day$PADDED_DATE_1.txt"

        curl "https://adventofcode.com/$YEAR/day/$DATE/input" -H "Cookie: session=$SESSION" >"inputs/day$PADDED_DATE.txt"

        # exit if target dir exists
        if [ -d "days/$PADDED_DATE" ]; then
            echo "folder for $DATE already exists"
            exit 1
        fi

        cp -r "days/00" "days/$PADDED_DATE"

        # replace the day number "day00" in Cargo.toml
        sed -i "s/day00/day$PADDED_DATE/g" "days/$PADDED_DATE/Cargo.toml"

        # and in main.rs change the vars
        sed -i "s/= 0;/= $DATE;/g" "days/$PADDED_DATE/src/main.rs"
      '';
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

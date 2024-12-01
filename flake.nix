{
  description = "Advent of code";

  inputs.fenix = {
    url = "github:nix-community/fenix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    fenix,
    ...
  }: let
    system = "x86_64-linux";
  in {
    devShells."${system}".default = let
      pkgs = import nixpkgs {
        inherit system;
      };

      setupday = pkgs.writeShellScriptBin "setupday" (builtins.readFile ./scripts/setupday.sh);
    in
      pkgs.mkShell {
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

          # go
          go
          go-tools

          setupday
        ];

        GO111MODULE = "on";

        # needed for running delve
        # https://github.com/go-delve/delve/issues/3085
        # https://nixos.wiki/wiki/C#Hardening_flags
        hardeningDisable = ["all"];

        shellHook = ''
          export WORKSPACE_DIR="$(git rev-parse --show-toplevel)"
        '';
      };
  };
}

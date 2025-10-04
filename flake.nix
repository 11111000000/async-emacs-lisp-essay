{
  description = "ERT tests for async Emacs Lisp examples (nix flake)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Emacs with required ELPA/MELPA packages on load-path
        emacsWithPkgs = pkgs.emacsWithPackages (epkgs: with epkgs; [
          async
          plz
          # request aio deferred are referenced in the article, add if needed:
          # request aio deferred
        ]);

        # Tools needed by tests (external commands used in examples)
        tools = with pkgs; [
          bash
          coreutils  # printf, yes, head, du, uname, etc.
          ripgrep
        ];

        PATH = pkgs.lib.makeBinPath tools;

        testFiles = pkgs.copyPathToStore ./tests;
        runner = pkgs.writeShellScriptBin "run-tests" ''
          set -euo pipefail
          export PATH=${PATH}:$PATH
          cd ${testFiles}
          exec ${emacsWithPkgs}/bin/emacs -Q --batch -l test-runner.el
        '';
      in
      {
        packages.default = runner;

        apps.test = {
          type = "app";
          program = "${runner}/bin/run-tests";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ emacsWithPkgs ] ++ tools;
          shellHook = ''
            echo "Run tests with: nix run .#test"
          '';
        };

        # Optional: enable `nix run` by default
        apps.default = self.apps.${system}.test;
      });
}

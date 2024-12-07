{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ocaml-overlay, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          name = "aoc_2024";
          version = "0.0.1";
        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.ocaml-overlay.overlays.default ];
          };
          devShells = {
            default = mkShell {
              buildInputs = [
                pkgs.just
                ocamlPackages.ocamlformat
                ocamlPackages.utop
                ocamlPackages.ocaml-lsp
                ocamlPackages.angstrom
                ocamlPackages.ppx_deriving
                ocamlPackages.re
                ocamlPackages.fmt
              ];
              inputsFrom = [ self'.packages.default ];
            };
          };

          packages = {
            default = buildDunePackage {
              inherit version;
              pname = name;
              propagatedBuildInputs = with ocamlPackages; [ core ];
              src = ./.;
            };
          };
        };
    };
}

{
  description = "Advent of Code Elixir environment";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          beam.packages.erlang_26.elixir
          beam.packages.erlang_26.erlang
          elixir-ls
        ];
      };
    };
}

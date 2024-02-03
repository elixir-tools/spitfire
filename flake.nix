{
  inputs = {nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";};

  outputs = {
    self,
    nixpkgs,
  }: let
    # Helper to provide system-specific attributes
    forAllSystems = f:
      nixpkgs.lib.genAttrs (system: let
        pkgs = nixpkgs.legacyPackages.${system};
        beamPackages = pkgs.beam_minimal.packages.erlang_26;
        elixir = beamPackages.elixir_1_16;
      in
        f {inherit system pkgs beamPackages elixir;});
  in {
    devShells = forAllSystems ({
      pkgs,
      beamPackages,
      elixir,
      ...
    }: {
      default = pkgs.mkShell {
        # The Nix packages provided in the environment
        packages = [beamPackages.erlang elixir];
      };
    });
  };
}

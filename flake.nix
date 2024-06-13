{
  description = "Spitfire parser for Elixir";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        beamPackages = pkgs.beam_minimal.packages.erlang_27;
        otp = (pkgs.beam.packagesWith beamPackages.erlang).extend (final: prev: {
          elixir_1_17 = prev.elixir_1_16.override {
            rev = "v1.17.0";
            # You can discover this using Trust On First Use by filling in `lib.fakeHash`
            sha256 = "sha256-RBylCfD+aCsvCqWUIvqXi3izNqqQoNfQNnQiZxz0Igg=";
            version = "1.17.0";
          };

          elixir = final.elixir_1_17;
          # This will get upstreamed into nix-beam-flakes at some point
          rebar = prev.rebar.overrideAttrs (_old: {doCheck = false;});
          rebar3 = prev.rebar3.overrideAttrs (_old: {doCheck = false;});
        });
        elixir = otp.elixir;
      in {
        devShells = {
          default = pkgs.mkShell {
            # The Nix packages provided in the environment
            packages = [
              beamPackages.erlang
              elixir
            ];
          };
        };
      };
    };
}

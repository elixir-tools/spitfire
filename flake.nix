{
  description = "spitfire";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    devshell.url = "github:numtide/devshell";
  };

  outputs = inputs @ {
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.devshell.flakeModule
      ];

      systems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              elixir = prev.elixir.overrideAttrs (old: {
                src = prev.fetchFromGitHub {
                  owner = "elixir-lang";
                  repo = "elixir";
                  rev = "52eaf1456182d5d6cce22a4f5c3f6ec9f4dcbfd9";
                  hash = "sha256-fOsV+jVIzsa38hQDvAjhUqee36nt8kG6AOpOQJnSZ74=";
                };
              });
            })
          ];
          config = {};
        };
        devshells.default = {
          packages = [
            pkgs.erlang_26
            pkgs.elixir
          ];
        };
      };
    };
}

{
  description = "spitfire";

  inputs = {
    beam-flakes.url = "github:shanesveller/nix-beam-flakes";
    beam-flakes.inputs.flake-parts.follows = "flake-parts";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs @ {
    beam-flakes,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [beam-flakes.flakeModule];

      systems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];

      perSystem = {
        config,
        pkgs,
        ...
      }: {
        beamWorkspace = {
          enable = true;
          devShell.languageServers.elixir = false;
          devShell.languageServers.erlang = false;
          flakePackages = true;
          pkgSet = let
            # Provide your desired OTP version here
            # erlang = pkgs.beam.interpreters.erlang_26;
            erlang = beam-flakes.lib.mkErlang pkgs "26.2.1" beam-flakes.lib.versions.erlang."26.2.1";
          in
            (pkgs.beam.packagesWith erlang).extend (final: prev: {
              elixir_1_17 = prev.elixir_1_16.override {
                rev = "514615d0347cb9bb513faa44ae1e36406979e516";
                # You can discover this using Trust On First Use by filling in `lib.fakeHash`
                sha256 = "sha256-lEnDgHi1sRg+3/JTnQJVo1qqSi0X2cNN4i9i9M95B2A=";
                version = "1.17.0-dev";
              };

              elixir = final.elixir_1_17;
              # This will get upstreamed into nix-beam-flakes at some point
              rebar = prev.rebar.overrideAttrs (_old: {doCheck = false;});
              rebar3 = prev.rebar3.overrideAttrs (_old: {doCheck = false;});
            });
          packages = {
            inherit (config.beamWorkspace.pkgSet) elixir erlang elixir-ls erlang-ls;
          };
          versions = {
            elixir = null;
            erlang = null;
          };
        };
      };
    };
}


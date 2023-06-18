{
  description = "diffuse";


  # Inputs
  # ======

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };


  # Outputs
  # =======

  outputs = { self, nixpkgs, flake-utils }:
    let
      inherit (nixpkgs.lib) attrValues optionalAttrs;

      # Make rosetta (x86) packages available on M1 macs
      nixpkgsConfig = {
        overlays = attrValues self.overlays ++ [(
          final: prev:
            (optionalAttrs
              (prev.stdenv.system == "aarch64-darwin")
              { inherit (final.pkgs-x86); }
            )
        )];
      };

      overlays = {
        apple-silicon = _: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            pkgs-x86 = import nixpkgs {
              system = "x86_64-darwin";
              inherit nixpkgsConfig;
            };
          };
      };

    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "diffuse";
      shell = ./nix/shell.nix;
      preOverlays = [ overlays.apple-silicon ];
    };
}
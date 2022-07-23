{
  description = "diffuse";


  # Inputs
  # ======

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };


  # Outputs
  # =======

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "diffuse";
      shell = ./nix/shell.nix;
      preOverlays = [ (import rust-overlay) ];
    };
}
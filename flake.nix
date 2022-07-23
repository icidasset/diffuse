{
  description = "diffuse";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    flake-utils.url = "github:numtide/flake-utils";

    # Dependencies
    # ------------
    rust-overlay.url = "github:oxalica/rust-overlay";

    # Getting build errors with watchexec from Nix channel
    # watchexec.url = "github:watchexec/watchexec";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "diffuse";
      shell = ./shell.nix;
      preOverlays = [ (import rust-overlay) ];
    };
}
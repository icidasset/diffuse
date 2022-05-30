let

  sources         = import ./nix/sources.nix;
  pkgs            = import sources.nixpkgs { overlays = [ (import rustOverlay) ]; };

  rustOverlay     = builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";

  # Dependencies
  # ------------

  deps = {

    tools = [
      pkgs.curl
      pkgs.just
      pkgs.simple-http-server
      pkgs.watchexec
    ];

    languages = [
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs.haskellPackages.stack
      pkgs.nodejs-18_x
      pkgs.nodePackages.pnpm

      pkgs.rust-bin.stable.latest.default
      # (pkgs.rust-bin.stable.latest.default.override {
      #   targets =
      #     if pkgs.stdenv.isDarwin then
      #       [ "aarch64-apple-darwin" "x86_64-apple-darwin" ]
      #     else
      #       [];
      # })
    ];

    tauri = {
      # Needed to build Tauri on Mac OS
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/darwin/apple-sdk/frameworks.nix
      macOS = [
        pkgs.darwin.apple_sdk.frameworks.AppKit
        pkgs.darwin.apple_sdk.frameworks.WebKit
        pkgs.libiconv
      ];
    };

  };

in

  pkgs.mkShell {

    buildInputs = builtins.concatLists [
      deps.tools
      deps.languages

      # Mac OS dependencies
      (pkgs.lib.optionals pkgs.stdenv.isDarwin deps.tauri.macOS)
    ];

  }

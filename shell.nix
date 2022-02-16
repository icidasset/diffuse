let

  # Install rust for Tauri build
  rustOverlay     = builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";

  sources         = import ./nix/sources.nix;
  pkgs            = import sources.nixpkgs { overlays = [ (import rustOverlay) ]; };
  frameworks      = pkgs.darwin.apple_sdk.frameworks;

in

  pkgs.mkShell {
    buildInputs = [

      # Dev Tools
      pkgs.curl
      pkgs.just
      pkgs.simple-http-server
      pkgs.watchexec

      # Language Specific
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs.haskellPackages.stack
      pkgs.nodejs-14_x
      pkgs.nodePackages.pnpm
      pkgs.rust-bin.stable.latest.default

      # Indirect dependencies
      frameworks.WebKit
      pkgs.gmp
      pkgs.libiconv

    ];

    # Workaround to get Tauri to work in a Nix environment on MacOS
    shellHook = ''
      export NIX_LDFLAGS="-F${frameworks.WebKit}/Library/Frameworks -framework WebKit $NIX_LDFLAGS";
    '';
  }

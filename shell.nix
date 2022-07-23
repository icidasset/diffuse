{ pkgs ? import <nixpkgs> }: let

  # Stack
  # -----
  # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/

  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

  # Dependencies
  # ------------

  deps = {

    tools = [
      pkgs.curl
      pkgs.just
      pkgs.simple-http-server
      # pkgs.watchexec
    ];

    languages = [
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs.nodejs-18_x
      pkgs.nodePackages.pnpm
      pkgs.rust-bin.stable.latest.default
      stack-wrapped
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

    NIX_PATH = "nixpkgs=" + pkgs.path;

  }

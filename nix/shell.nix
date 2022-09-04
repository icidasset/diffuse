{ pkgs ? import <nixpkgs> {} }: with pkgs; let

  # Rust
  # ----

  rust = rust-bin.stable.latest.default;
  rustPlatform = makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };

  # Wraps
  # -----
  # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/

  stack-wrapped = symlinkJoin {
    name = "stack";
    paths = [ haskellPackages.stack ];
    buildInputs = [ makeWrapper ];
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
      curl
      just
      simple-http-server
      pkgs-x86.watchexec
    ];

    languages = [
      elmPackages.elm
      elmPackages.elm-format
      nodejs-18_x
      nodePackages.pnpm
      rust
      stack-wrapped
    ];

    tauri = {
      # Needed to build Tauri on Mac OS
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/darwin/apple-sdk/frameworks.nix
      macOS = [
        darwin.apple_sdk.frameworks.AppKit
        darwin.apple_sdk.frameworks.WebKit
        libiconv
      ];
    };

  };

in

mkShell {

  buildInputs = builtins.concatLists [
    deps.tools
    deps.languages

    # Mac OS dependencies
    (lib.optionals stdenv.isDarwin deps.tauri.macOS)
  ];

  NIX_PATH = "nixpkgs=" + path;

}

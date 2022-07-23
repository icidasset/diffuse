{ pkgs ? import <nixpkgs> {} }: with pkgs; let

  # Rust
  # ----

  rust = rust-bin.stable.latest.default;
  rustPlatform = makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };

  # Trying to fix an issue with `watchexec` on MacOS
  # https://github.com/h4llow3En/mac-notification-sys/issues/28
  # Nix uses old Apple SDKs? Not sure, but definitely a Nix issue.
  watchexec = rustPlatform.buildRustPackage rec {
    pname = "watchexec";
    version = "1.20.4";

    cargoSha256 = "sha256-YM+Zm3wFp3Lsx5LmyjGwZywV/SZjriL6JMDO1l0tNf4=";

    src = fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "cli-v${version}";
      sha256 = "sha256-se3iqz+qjwf71wvHQhCWYryEdUc+kY0Q0ZTg4i1ayNI=";
    };

    nativeBuildInputs = lib.optionals
      stdenv.isDarwin
      (with darwin.apple_sdk_11_0.frameworks; [ Cocoa Foundation ]);
  };

  # Wraps
  # -----
  # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/

  stack-wrapped = symlinkJoin {
    name = "stack";
    paths = [ stack ];
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
      # watchexec
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

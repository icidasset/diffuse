{ ghc }:
with (import <nixpkgs> {});
let
  macos =
    if stdenv.isDarwin then
      [
        darwin.apple_sdk.frameworks.CoreServices
        darwin.apple_sdk.frameworks.Foundation
        darwin.apple_sdk.frameworks.Cocoa
      ]
    else
      [];
in
haskell.lib.buildStackProject {
  inherit ghc;
  name = "diffuse";
  buildInputs = [
    lzma
    openssl
    postgresql
    zlib
  ] ++ macos;
}
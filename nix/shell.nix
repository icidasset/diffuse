{ pkgs ? import <nixpkgs> {} }: with pkgs; let

  isM1Mac = stdenv.system == "aarch64-darwin";

  # Dependencies
  # ------------

  deps = {

    tools = [
      curl
      just
      (if isM1Mac then pkgs-x86.miniserve else miniserve)
      (if isM1Mac then pkgs-x86.watchexec else watchexec)
    ];

    languages = [
      elmPackages.elm
      elmPackages.elm-format
      nodejs-18_x
      stack
    ];

  };

in

mkShell {

  buildInputs = builtins.concatLists [
    deps.tools
    deps.languages
  ];

  NIX_PATH = "nixpkgs=" + path;

}

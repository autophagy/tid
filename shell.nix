{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f096b7122ab08e93c8b052c92461ca71b80c0cc8.tar.gz") {}
}:
with pkgs;

let
  basePackages = [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    elmPackages.elm-review
    elmPackages.elm-test
  ];

  inputs = basePackages;

in mkShell {
  buildInputs = inputs;
}

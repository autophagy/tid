{ pkgs ? import <nixpkgs> { } }:

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

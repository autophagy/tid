{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f096b7122ab08e93c8b052c92461ca71b80c0cc8.tar.gz") {}
}:
let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "5b9e0ff9d3b551234b4f3eb3983744fa354b17f1";
    sha256 = "sha256:01l4phiqgw9xgaxr6jr456qmww6kzghqrnbc7aiiww3h6db5vw53";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in pkgs.stdenv.mkDerivation {
  pname = "tid";
  version = "0.1.0";
  src = gitignoreSource ./.;

  buildInputs = [ pkgs.elmPackages.elm pkgs.nodePackages.uglify-js ];

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };

  installPhase = ''
    mkdir -p $out/share/doc
    cp -r public/* $out
    elm make ./src/Main.elm --output $out/Main.js --docs $out/share/doc/Main.json --optimize

    uglifyjs $out/Main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        | uglifyjs --mangle --output $out/Main.js
  '';
}


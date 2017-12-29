{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "argonaut";

  buildInputs = [
    simpleBuildTool
    nodejs
  ];
}

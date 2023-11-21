{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [ clisp sbcl rlwrap ];
}

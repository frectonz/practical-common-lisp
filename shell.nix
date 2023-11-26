{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs =
    let
      swank = pkgs.writeShellScriptBin "swank" ''
        ${pkgs.roswell}/bin/ros run --eval '(ql:quickload :swank)'  --eval '(swank:create-server :dont-close t)'
      '';
    in
    with pkgs; [ clisp sbcl rlwrap swank ];
}

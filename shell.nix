{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = (with pkgs; [
    spago
    pulp
    nodejs-14_x
    nodePackages.bower
    purescript
  ]);
}
{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = (with pkgs; [
    spago
    pulp
    nodejs
    nodePackages.bower
    purescript
  ]);
}

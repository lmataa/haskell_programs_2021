{ pkgs ? (import <nixpkgs> {}).pkgs }:
with pkgs.haskellPackages;

pkgs.mkShell{
  buildInputs = [
	pkgs.ghc
	];
}


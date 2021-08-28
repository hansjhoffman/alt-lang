{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  haskellDeps = ps: with ps; [
    base
    hlint
    parsec_3_1_14_0
  ];
  
  ghc = haskell.packages.ghc8104.ghcWithPackages haskellDeps;
  
  inputs = [
    gcc
    ghc
    stack
    llvm
    nixfmt
  ];
  
  hooks = ''
    mkdir -p .nix-stack
    export STACK_ROOT=$PWD/.nix-stack
  '';
in 
  stdenv.mkDerivation {
    name = "alt-lang";
    src = ./.;
    buildInputs = inputs;
    shellHook = hooks;
  }
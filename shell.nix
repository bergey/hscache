{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellngPackages }:

let 
  hs = haskellPackages.override {
        overrides = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/${pkg}/${version}.nix" {};
          thisPackage = self.callPackage ./. {};
      };
    };
  in (hs.thisPackage.override (args: args // {
    mkDerivation = expr: args.mkDerivation (expr // {
      buildTools = [  hs.cabal-install ];
    });
  }))

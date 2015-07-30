{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, QuickCheck, quickspec, stdenv }:
      mkDerivation {
        pname = "QuickSpecMeasure";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ array base QuickCheck quickspec ];
        homepage = "http://chriswarbo.net/git/quickspec-measure";
        description = "Benchmarking QuickSpec";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

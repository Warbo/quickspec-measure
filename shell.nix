{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, QuickCheck, quickspec, stdenv }:
      mkDerivation {
        pname = "QuickSpecMeasure";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ array base QuickCheck quickspec ];
        homepage = "http://chriswarbo.net/git/quickspec-measure";
        description = "Benchmarking QuickSpec";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

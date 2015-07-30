{ mkDerivation, array, base, QuickCheck, quickspec, stdenv }:
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
}

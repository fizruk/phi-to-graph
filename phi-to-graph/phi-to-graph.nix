{ mkDerivation, array, base, Cabal, cabal-doctest, doctest, Glob
, lib, logict, mtl, QuickCheck, template-haskell
}:
mkDerivation {
  pname = "phi-to-graph";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ array base logict mtl ];
  executableHaskellDepends = [ array base logict mtl ];
  testHaskellDepends = [
    array base doctest Glob logict mtl QuickCheck template-haskell
  ];
  homepage = "https://github.com/fizruk/phi-to-graph#readme";
  license = lib.licenses.bsd3;
}

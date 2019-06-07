{ mkDerivation, base, brick, cabal-install, containers, hindent
, hlint, lens, pretty-simple, stdenv, text, vty
}:
mkDerivation {
  pname = "minicity";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brick containers lens pretty-simple text vty
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  homepage = "https://github.com/pmiddend/minicity";
  description = "Build a city, keep it in balance";
  license = stdenv.lib.licenses.gpl3;
}

{ mkDerivation, attoparsec, base, bytestring, Cabal, file-embed
, hastache, optparse-applicative, process, shelly, split, stdenv
, system-fileio, system-filepath, text
}:
mkDerivation {
  pname = "hscache";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring Cabal file-embed hastache
    optparse-applicative process shelly split system-fileio
    system-filepath text
  ];
  description = "Sandboxed cabal builds with cached dependencies";
  license = stdenv.lib.licenses.bsd3;
}

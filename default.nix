{ mkDerivation, array, base, containers, data-default, lens
, monad-logger, mtl, process, semigroupoids, setlocale, stdenv
, text, transformers, unix, xhb, xhb-atom-cache, xhb-event-queue
, xhb-keysyms, xhb-mapping-state, xhb-monad
}:
mkDerivation {
  pname = "wmonad";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers data-default lens monad-logger mtl process
    semigroupoids setlocale text transformers unix xhb xhb-atom-cache
    xhb-event-queue xhb-keysyms xhb-mapping-state xhb-monad
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers lens mtl ];
  license = stdenv.lib.licenses.mit;
}

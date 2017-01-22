{ mkDerivation, base, containers, lens, monad-logger, mtl, process
, semigroupoids, setlocale, stdenv, text, transformers, unix, xhb
, xhb-atom-cache, xhb-event-queue, xhb-keysyms, xhb-mapping-state
, xhb-monad
}:
mkDerivation {
  pname = "wmonad";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens monad-logger mtl process semigroupoids
    setlocale text transformers unix xhb xhb-atom-cache xhb-event-queue
    xhb-keysyms xhb-mapping-state xhb-monad
  ];
  license = stdenv.lib.licenses.mit;
}

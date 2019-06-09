{ mkDerivation, base, containers, ghcjs-dom, jsaddle, jsaddle-warp
, lens, mtl, random, reflex, reflex-dom, reflex-dom-canvas, stdenv
, text, time
}:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers ghcjs-dom jsaddle jsaddle-warp lens mtl random
    reflex reflex-dom reflex-dom-canvas text time
  ];
  license = stdenv.lib.licenses.bsd3;
}

{ lib }:
import ./mkDerivation.nix {
  pname = "purs-backend-es";
  exe = "purs-backend-es";
  js = "index.js";
  meta = {
    description = "An optimizing backend toolkit for PureScript's CoreFn.";
    homepage = "https://github.com/aristanetworks/purescript-backend-optimizer";
    license = lib.licenses.mit;
  };
}

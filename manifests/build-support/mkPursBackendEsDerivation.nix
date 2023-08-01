{ lib }:
import ./mkDerivation.nix {
  name = "purs-backend-es";
  js = "index.js";
  meta = {
    description = "An optimizing backend toolkit for PureScript's CoreFn.";
    homepage = "https://github.com/aristanetworks/purescript-backend-optimizer";
    license = lib.licenses.mit;
  };
}

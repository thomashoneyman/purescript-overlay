{ lib }:
import ./mkDerivation.nix {
  name = "spago";
  js = "bin/bundle.js";
  meta = {
    description = "PureScript package manager and build tool";
    homepage = "https://github.com/purescript/spago";
    license = lib.licenses.bsd3;
  };
}
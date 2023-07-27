{ lib }:
import ./mkDerivation.nix {
  pname = "spago";
  exe = "spago";
  js = "bin/bundle.js";
  meta = {
    description = "PureScript package manager and build tool";
    homepage = "https://github.com/purescript/spago";
    license = lib.licenses.bsd3;
  };
}
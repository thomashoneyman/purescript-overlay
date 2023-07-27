{ lib }:
import ./mkDerivation.nix {
  pname = "purs-tidy";
  exe = "purs-tidy";
  js = "bin/index.js";
  meta = {
    description = "PureScript formatter and tidy-upper";
    homepage = "https://github.com/natefaubion/purescript-tidy";
    license = lib.licenses.mit;
  };
}

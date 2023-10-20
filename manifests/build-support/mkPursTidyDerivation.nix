import ./mkDerivationBasic.nix {
  name = "purs-tidy";
  js = "bin/index.js";
  meta = lib: {
    description = "PureScript formatter and tidy-upper";
    homepage = "https://github.com/natefaubion/purescript-tidy";
    license = lib.licenses.mit;
  };
}

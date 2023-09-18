import ./mkDerivation.nix {
  name = "purs-language-server";
  js = "server.js";
  meta = lib: {
    description = "A Node-based language server protocol for PureScript";
    homepage = "https://github.com/nwolverson/purescript-language-server";
    license = lib.licenses.mit;
  };
}

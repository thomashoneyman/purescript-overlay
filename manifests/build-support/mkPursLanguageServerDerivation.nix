{
  stdenv,
  fetchurl,
  lib,
  nodejs,
  writeText,
}: {
  version,
  url,
  hash,
}:
stdenv.mkDerivation rec {
  pname = "purescript-language-server";
  inherit version;

  src = fetchurl {inherit url hash;};

  nativeBuildInputs = [nodejs];

  # We are just downloading a file, so we need to ensure we do not attempt to
  # unpack it as if it were a tarball.
  dontUnpack = true;

  packageJSON = writeText "package.json" ''
    {
      "name": "purescript-language-server",
      "version": "${version}"
    }
  '';

  installPhase = ''
    PACKAGE=$out/node_modules/${pname}
    mkdir -p $PACKAGE
    cp $src $PACKAGE/purescript-language-server.js
    cp ${packageJSON} $PACKAGE/package.json

    BIN=$PACKAGE/purescript-language-server.js
    chmod +x $BIN
    patchShebangs $BIN

    mkdir -p $out/bin
    ln -s $BIN $out/bin/${pname}
  '';

  meta = {
    description = "A Node-based language server protocol for PureScript";
    homepage = "https://github.com/nwolverson/purescript-language-server";
    license = lib.licenses.mit;
  };
}

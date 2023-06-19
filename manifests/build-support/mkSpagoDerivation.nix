{
  lib,
  stdenv,
  fetchurl,
  nodejs,
}: {
  version,
  url,
  hash,
}:
stdenv.mkDerivation rec {
  pname = "spago";
  inherit version;

  src = fetchurl {inherit url hash;};

  nativeBuildInputs = [nodejs];

  buildPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out/bin

    mv package/bin/bundle.js $out/bin/bundle.js
    mv package/package.json $out/package.json

    chmod +x $out/bin/bundle.js
    patchShebangs $out/bin/bundle.js

    ln -s $out/bin/bundle.js $out/bin/spago
  '';

  meta = with lib; {
    description = "PureScript package manager and build tool";
    homepage = "https://github.com/purescript/spago";
    license = licenses.bsd3;
  };
}

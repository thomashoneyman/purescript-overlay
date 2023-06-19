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
  pname = "purs-backend-es";
  inherit version;

  src = fetchurl {inherit url hash;};

  nativeBuildInputs = [nodejs];

  buildPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out/bin

    mv package/* $out

    chmod +x $out/index.js
    patchShebangs $out/index.js

    ln -s $out/index.js $out/bin/purs-backend-es
  '';

  meta = with lib; {
    description = "PureScript formatter and tidy-upper";
    homepage = "https://github.com/natefaubion/purescript-tidy";
  };
}

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
  pname = "purs-tidy";
  inherit version;

  src = fetchurl {inherit url hash;};

  nativeBuildInputs = [nodejs];

  buildPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out/bin

    mv package/* $out

    chmod +x $out/bin/index.js
    patchShebangs $out/bin/index.js

    ln -s $out/bin/index.js $out/bin/purs-tidy
  '';

  meta = with lib; {
    description = "PureScript formatter and tidy-upper";
    homepage = "https://github.com/natefaubion/purescript-tidy";
  };
}

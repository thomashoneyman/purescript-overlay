{
  name,
  js,
  meta,
}: {
  lib,
  stdenv,
  fetchurl,
  nodejs,
}: version: source:
stdenv.mkDerivation {
  pname = name;
  inherit version;

  src = fetchurl source;

  nativeBuildInputs = [nodejs];

  buildPhase = ''
    tar xf $src
  '';

  installPhase = ''
    PACKAGE=$out/node_modules/${name}
    mkdir -p $PACKAGE
    mv package/* $PACKAGE

    BIN=$PACKAGE/${js}
    chmod +x $BIN
    patchShebangs $BIN

    mkdir -p $out/bin
    ln -s $BIN $out/bin/${name}
  '';

  meta = meta lib // {mainProgram = name;};
}

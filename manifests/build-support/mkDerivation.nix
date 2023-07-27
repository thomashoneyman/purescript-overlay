{
  pname,
  exe,
  js,
  meta
}:
{
  stdenv,
  fetchurl,
  nodejs,
}:
{
  version,
  url,
  hash,
}:
stdenv.mkDerivation rec {
  inherit pname version;

  src = fetchurl {inherit url hash;};

  nativeBuildInputs = [nodejs];

  buildPhase = ''
    tar xf $src
  '';

  installPhase = ''
    PACKAGE=$out/node_modules/${pname}
    mkdir -p $PACKAGE
    mv package/* $PACKAGE

    BIN=$PACKAGE/${js}
    chmod +x $BIN
    patchShebangs $BIN

    mkdir -p $out/bin
    ln -s $BIN $out/bin/${exe}
  '';

  inherit meta;
}

{
  stdenv,
  lib,
  zlib,
  gmp,
  fetchurl,
  ncurses5,
}:
{
  version,
  url,
  hash,
}:
let
  dynamic-linker = stdenv.cc.bintools.dynamicLinker;

  # Versions >= 0.15.16-7 are statically linked and don't need patching
  needsPatching = lib.versionOlder version "0.15.16-7";

  patchelf =
    libPath:
    if stdenv.isDarwin || !needsPatching then
      ""
    else
      ''
        chmod u+w $PURS
        patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS
        chmod u-w $PURS
      '';
in
stdenv.mkDerivation rec {
  pname = "purs";
  inherit version;

  src = fetchurl { inherit url hash; };

  buildInputs = lib.optionals needsPatching [
    zlib
    gmp
    ncurses5
  ];
  libPath = lib.makeLibraryPath buildInputs;
  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    PURS="$out/bin/purs"
    install -D -m555 -T purs $PURS
    ${patchelf libPath}
    mkdir -p $out/etc/bash_completion.d/
    $PURS --bash-completion-script $PURS > $out/etc/bash_completion.d/purs-completion.bash
  '';

  meta = {
    description = "Compiler for a strongly-typed language that compiles to JavaScript";
    homepage = "https://github.com/purescript/purescript";
    mainProgram = pname;
  };
}

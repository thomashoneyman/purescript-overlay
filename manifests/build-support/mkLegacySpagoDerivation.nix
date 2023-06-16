{
  stdenv,
  lib,
  zlib,
  gmp,
  fetchurl,
  ncurses5,
}: {
  version,
  url,
  hash,
}: let
  dynamic-linker = stdenv.cc.bintools.dynamicLinker;

  patchelf = libPath:
    if stdenv.isDarwin
    then ""
    else ''
      chmod u+w $PURS
      patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS
      chmod u-w $PURS
    '';
in
  stdenv.mkDerivation rec {
    pname = "spago";
    inherit version;

    src = fetchurl {inherit url hash;};

    buildInputs = [zlib gmp ncurses5 stdenv.cc.cc.lib];
    libPath = lib.makeLibraryPath buildInputs;
    dontStrip = true;

    installPhase = ''
      mkdir -p $out/bin
      tar xf $src -C $out/bin

      SPAGO="$out/bin/spago"
      ${patchelf libPath}

      mkdir -p $out/etc/bash_completion.d/
      $SPAGO --bash-completion-script $SPAGO > $out/etc/spago-completion.d/purs-completion.bash
    '';

    meta = with lib; {
      description = "Package manager for PureScript";
      homepage = "https://github.com/purescript/spago";
    };
  }

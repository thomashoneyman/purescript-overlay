{
  stdenv,
  lib,
  zlib,
  gmp,
  ncurses,
  # extra arguments
  version,
  src,
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
    pname = "purescript";
    inherit version src;

    buildInputs = [zlib gmp ncurses];
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

    meta.description = "A strongly-typed functional programming language that compiles to JavaScript";
  }

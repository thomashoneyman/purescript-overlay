{
  system,
  stdenv,
  lib,
  zlib,
  gmp,
  fetchurl,
  # extra arguments
  ncurses,
  version,
  tarballs,
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
    inherit version;
    
    src =
      if builtins.hasAttr system tarballs
      then (fetchurl tarballs.${system})
      else if system == "aarch64-darwin"
      then let
        arch = "x86_64-darwin";
        msg = "Using the non-native ${arch} binary. While this binary may run under Rosetta 2 translation, no guarantees can be made about stability or performance.";
      in
        lib.warn msg (fetchurl tarballs.${arch})
      else throw "Architecture not supported: ${system}";

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

    meta = with lib; {
      description = "Compiler for a strongly-typed language that compiles to JavaScript";
      homepage = "https://github.com/purescript/purescript";
    };
  }

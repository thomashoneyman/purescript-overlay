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
  patchelf = libPath:
    if stdenv.isDarwin
    then ""
    else ''
      chmod u+w $SPAGO
      patchelf --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" --set-rpath ${libPath} $SPAGO
      chmod u-w $SPAGO
    '';
in
  stdenv.mkDerivation rec {
    pname = "spago";
    inherit version;

    src = fetchurl {inherit url hash;};

    buildInputs = [zlib gmp ncurses5 stdenv.cc.cc.lib];
    libPath = lib.makeLibraryPath buildInputs;
    dontStrip = true;

    unpackPhase = ''
      mkdir -p $out/bin
      export XDG_CACHE_HOME=$(mktemp -d)

      tar xf $src -C $out/bin

      SPAGO=$out/bin/spago
      ${patchelf libPath}

      mkdir -p $out/etc/bash_completion.d/
      $SPAGO --bash-completion-script $SPAGO > $out/etc/bash_completion.d/spago-completion.bash

      rm -rf $XDG_CACHE_HOME
    '';

    dontInstall = true;

    meta = with lib; {
      description = "Package manager for PureScript";
      homepage = "https://github.com/purescript/spago";
    };
  }

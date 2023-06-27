{
  stdenv,
  purix,
}: let
  lock = purix.buildSpagoLock {
    src = ./.;
    lockfile = ./spago.lock;
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      echo "Linking ..."
      ln -s ${lock.simple}/output .
    '';
    installPhase = ''
      mkdir -p $out
      cp -r output $out
    '';
  }

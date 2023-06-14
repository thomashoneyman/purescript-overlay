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
      echo ${lock.simple.dependencies.globs}
      touch $out
    '';
    installPhase = ''
      touch $out
    '';
  }

{
  stdenv,
  purix,
}: let
  packageLock = purix.buildPackageLock {src = ./.;};
  spagoLock = purix.buildSpagoLock {
    src = ./.;
    lockfile = ./spago.lock;
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      ln -s ${packageLock}/js/node_modules .
      echo ${spagoLock.simple.dependencies.globs}
      touch $out
    '';
    installPhase = ''
      touch $out
    '';
  }

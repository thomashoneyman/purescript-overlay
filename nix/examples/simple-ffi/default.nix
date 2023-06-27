{
  stdenv,
  purix,
  purs-bin,
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
    nativeBuildInputs = [purs-bin.purs-0_15_9];
    buildPhase = ''
      echo "Linking ..."
      ln -s ${packageLock}/js/node_modules .
      ln -s ${spagoLock.simple}/output .
    '';
    installPhase = ''
      ls output > $out
    '';
  }

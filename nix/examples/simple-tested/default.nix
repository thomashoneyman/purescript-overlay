{
  stdenv,
  purix,
  slimlock,
}: let
  locked = purix.buildSpagoLock {
    src = ./.;
    lockfile = ./spago.lock;
    tests.simple-tested = "Example.Simple.Tested.Tests";
    npmDependencies = slimlock.buildPackageLock {src = ./.;} + "/js/node_modules";
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      echo "Linking ..."
      ln -s ${locked.simple-tested}/output .
    '';
    installPhase = ''
      mkdir -p $out
      cp -r output $out
    '';
  }

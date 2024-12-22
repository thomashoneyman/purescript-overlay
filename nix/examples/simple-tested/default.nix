{
  stdenv,
  purix,
}: let
  locked = purix.buildSpagoLock {
    src = ./.;
    lockfile = ./spago.lock;
    tests.simple-tested = "Example.Simple.Tested.Tests";
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      echo "Linking ..."
      ln -s ${locked.jsArtifacts.simple-tested}/output .
    '';
    installPhase = ''
      mkdir -p $out
      cp -r output $out
    '';
  }

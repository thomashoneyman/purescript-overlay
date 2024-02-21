{
  stdenv,
  purix,
}: let
  generated = stdenv.mkDerivation {
    name = "generated";
    src = ./.;
    installPhase = ''
      touch Generated.purs
      echo 'module Generated where' >> Generated.purs
      echo 'import Prelude' >> Generated.purs
      echo 'answer :: Int' >> Generated.purs
      echo 'answer = 42' >> Generated.purs
      mkdir -p $out
      cp Generated.purs $out
    '';
  };

  lock = purix.buildSpagoLock {
    src = ./.;
    lockfile = ./spago.lock;
    extraSrcs.simple-codegen = generated;
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      echo "Linking ..."
      ln -s ${lock.simple-codegen}/output .
    '';
    installPhase = ''
      mkdir -p $out
      cp -r output $out
    '';
  }

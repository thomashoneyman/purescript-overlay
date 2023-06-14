{
  stdenv,
  purs,
  buildSpagoLock,
}: let
  lock = buildSpagoLock {
    inherit purs;
    src = ./.;
  };
in
  stdenv.mkDerivation {
    name = "bin";
    src = ./.;
    buildPhase = ''
      exit 1
      touch $out
    '';
    installPhase = ''
      touch $out
    '';
  }

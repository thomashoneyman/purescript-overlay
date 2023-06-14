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
      touch $out
    '';
    installPhase = ''
      touch $out
    '';
  }

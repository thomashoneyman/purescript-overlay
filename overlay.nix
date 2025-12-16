final: prev:
import ./manifests {
  inherit (prev)
    system
    callPackage
    stdenv
    ;
}

final: prev:
import ./manifests {
  inherit (prev)
    lib
    callPackage
    nodejs
    nodejs_20
    python3
    darwin
    nodePackages
    installShellFiles
    makeWrapper
    stdenv
    ;
  system = prev.stdenv.hostPlatform.system;
}

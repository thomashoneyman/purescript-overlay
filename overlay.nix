final: prev:
let
  fromYAML = prev.callPackage ./nix/from-yaml.nix { };

  # All of the tools supported by this repo
  tooling = import ./manifests {
    inherit (prev)
      system
      callPackage
      stdenv
      ;
  };

  # All of the library functions supported by this repo
  buildSpagoLock = prev.callPackage ./nix/build-spago-lock.nix { inherit fromYAML; };

  purix = {
    buildSpagoLock = buildSpagoLock.buildSpagoLock;
  };
in
{ purix = purix; } // tooling

final: prev: let
  fromYAML = prev.callPackage ./nix/from-yaml.nix {};

  # All of the tools supported by this repo
  tooling =
    import ./manifests {inherit (prev) system lib callPackage stdenv;};

  # All of the library functions supported by this repo
  buildPackageLock = prev.callPackage ./nix/build-package-lock.nix {};
  buildSpagoLock =
    prev.callPackage ./nix/build-spago-lock.nix {inherit fromYAML;};

  purix = {
    lib = buildPackageLock // buildSpagoLock;
    buildPackageLock = buildPackageLock.buildPackageLock;
    buildSpagoLock = buildSpagoLock.buildSpagoLock;
  };
in
  {purix = purix;} // tooling

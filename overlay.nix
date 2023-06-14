final: prev: let
  fromYAML = prev.callPackage ./nix/from-yaml.nix {};

  # All of the tools supported by this repo
  tooling = import ./manifests {inherit (prev) callPackage callPackages;};

  # All of the library functions supported by this repo
  library = {
    buildPackageLock = prev.callPackage ./nix/package-lock.nix {};
    buildSpagoLock = prev.callPackage ./nix/spago-lock.nix {inherit fromYAML;};
  };
in
  tooling // library

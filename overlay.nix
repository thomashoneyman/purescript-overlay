final: prev: let
  fromYAML = prev.callPackage ./nix/from-yaml.nix {};

  # All of the tools supported by this repo
  tooling = import ./manifests {inherit (prev) callPackage callPackages;};

  # All of the library functions supported by this repo
  buildPackageLock = prev.callPackage ./nix/package-lock.nix {};
  buildSpagoLock = prev.callPackage ./nix/spago-lock.nix {inherit fromYAML;};
  purix = {
    lib = buildPackageLock // buildSpagoLock;
    buildPackageLock = buildPackageLock.buildPackageLock;
    buildSpagoLock = buildSpagoLock.workspaces;
  };
in
  {inherit purix;} // tooling

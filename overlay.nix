final: prev: let
  fromYAML = prev.callPackage ./build-support/from-yaml.nix {};
in rec {
  # PureScript tools
  purs = prev.callPackages ./purs/versions.nix {};
  spago = prev.callPackages ./spago/versions.nix {compilers = purs;};

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./build-support/package-lock.nix {};
  buildSpagoLock = prev.callPackage ./build-support/spago-lock.nix {
    inherit fromYAML;
  };
}

final: prev: let
  fromYAML = prev.callPackage ./nix/lib/fromYAML.nix {};
  parseSemVer = prev.callPackage ./nix/lib/parseVersion.nix {};
in {
  # PureScript tools
  purs = prev.callPackages ./nix/purs/purs.nix {};
  spago = prev.callPackages ./nix/spago/spago.nix {inherit parseSemVer;};

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./nix/lib/buildPackageLock.nix {};
  buildSpagoLock = prev.callPackage ./nix/lib/buildSpagoLock.nix {inherit fromYAML;};
}

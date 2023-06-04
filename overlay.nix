final: prev: let
  fromYAML = prev.callPackage ./nix/lib/fromYAML.nix {};
in {
  # PureScript tools
  purs = prev.callPackages ./nix/purs.nix {};
  spago = prev.callPackages ./nix/spago.nix {purs = final.purs;};

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./nix/lib/buildPackageLock.nix {};
  buildSpagoLock = prev.callPackage ./nix/lib/buildSpagoLock.nix {inherit fromYAML;};
}

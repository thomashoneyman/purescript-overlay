final: prev: let
  fromYAML = prev.callPackage ./nix/lib/fromYAML.nix {};
  mkPursDerivation = prev.callPackage ./nix/purs/mkPursDerivation.nix {};
  mkSpagoDerivation = prev.callPackage ./nix/spago/mkSpagoDerivation.nix {};
in {
  # PureScript tools
  purs = prev.callPackages ./nix/purs/purs.nix {inherit mkPursDerivation;};
  spago = prev.callPackages ./nix/spago/spago.nix {inherit mkSpagoDerivation;};

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./nix/lib/buildPackageLock.nix {};
  buildSpagoLock = prev.callPackage ./nix/lib/buildSpagoLock.nix {inherit fromYAML;};
}

final: prev:
let fromYAML = prev.callPackage ./nix/build-support/from-yaml.nix { };
in {
  # PureScript tools
  purs = prev.callPackages ./versions-purs.nix { };
  spago = prev.callPackages ./versions-spago.nix { purs = final.purs; };

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./nix/build-support/package-lock.nix { };
  buildSpagoLock = prev.callPackage ./nix/build-support/spago-lock.nix { inherit fromYAML; };
}

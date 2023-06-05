final: prev: let
  fromYAML = prev.callPackage ./nix/lib/fromYAML.nix {};
  named = builtins.fromJSON (builtins.readFile ./manifests/named.json);
  mkManifestDerivations = bin: prev.callPackages ./nix/mkManifestDerivations.nix {inherit bin;};
in {
  # PureScript tools
  purs-bin = mkManifestDerivations "purs";
  purs = final.purs-bin.${named.purs};
  purs-unstable = final.purs-bin.${named.purs-unstable};

  spago-bin = mkManifestDerivations "spago";
  spago = final.spago-bin.${named.spago};

  # Utilities for building PureScript packages
  buildPackageLock = prev.callPackage ./nix/lib/buildPackageLock.nix {};
  buildSpagoLock = prev.callPackage ./nix/lib/buildSpagoLock.nix {inherit fromYAML;};
}

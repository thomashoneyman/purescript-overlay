final: prev: let
  fromYAML = prev.callPackage ./nix/lib/fromYAML.nix {};

  mkManifestDerivations = bin: prev.callPackages ./nix/mkManifestDerivations.nix {inherit bin;};

  # The 'named.json' file records what packages should be mapped to the default
  # package names. This is the latest stable or unstable version for each package.
  namedPackages = builtins.mapAttrs (name: value: let
    bin-name = "${builtins.replaceStrings ["-unstable"] [""] name}-bin";
  in
    final.${bin-name}.${value})
  (builtins.fromJSON (builtins.readFile ./manifests/named.json));
in
  {
    # PureScript tools
    purs-bin = mkManifestDerivations "purs";
    spago-bin = mkManifestDerivations "spago";

    # Utilities for building PureScript packages
    buildPackageLock = prev.callPackage ./nix/lib/buildPackageLock.nix {};
    buildSpagoLock = prev.callPackage ./nix/lib/buildSpagoLock.nix {inherit fromYAML;};
  }
  // namedPackages

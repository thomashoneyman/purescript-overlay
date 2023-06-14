# This derivation exposes all the tools described by the manifests in this
# directory.
{
  callPackage,
  callPackages,
}: let
  fromYAML = callPackage ../nix/from-yaml.nix {};
  mkManifestDerivations = bin: callPackages ./build-support/mkManifestDerivations.nix {inherit bin;};

  packages = {
    purs-bin = mkManifestDerivations "purs";
    spago-bin = mkManifestDerivations "spago";
  };

  # The 'named.json' file records what packages should be mapped to the default
  # package names. This is the latest stable or unstable version for each package.
  namedPackages =
    builtins.mapAttrs
    (
      name: value: let
        bin-name = "${builtins.replaceStrings ["-unstable"] [""] name}-bin";
      in
        packages.${bin-name}.${value}
    )
    (builtins.fromJSON (builtins.readFile ./named.json));
in
  namedPackages // packages

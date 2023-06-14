# This derivation exposes all the tools described by the manifests in this
# directory.
{
  lib,
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
    lib.mapAttrs'
    (
      name: value: let
        # We strip the 'stable' suffix from package names to get the default
        # package name.
        pkg-name = builtins.replaceStrings ["-stable"] [""] name;
        # The bin name is the default package name with '-bin' appended.
        bin-name = "${builtins.replaceStrings ["-stable" "-unstable"] ["" ""] name}-bin";
      in {
        name = pkg-name;
        value = packages.${bin-name}.${value};
      }
    )
    (builtins.fromJSON (builtins.readFile ./named.json));
in
  namedPackages // packages

{
  lib,
  fetchurl,
  writeTextFile,
  nodejs,
  stdenv,
}: rec {
  # Read a package-lock.json as a Nix attrset
  readPackageLock = lockfile: builtins.fromJSON (builtins.readFile lockfile);

  # Read the dependencies listed in a package-lock.json file where each key is
  # the name of a dependency and each value is an attrset with the following
  # keys:
  #   - version: The version of the dependency
  #   - integrity: The sri hash of the dependency
  #   - resolved: The URL of the tarball for the dependency from the NPM registry
  getDependencies = lock: let
    # Don't include the current package, as it isn't a dependency.
    removeCurrent = packages: removeAttrs packages [""];

    # Remove node_modules prefixes from package names listed in the lockfile and
    # ensure deps have all required fields.
    normalizePackage = name: value: {
      name = lib.last (lib.strings.splitString "node_modules/" name);
      value = {
        version = value.version or (throw "Dependency ${name} does not have a 'version' key");
        integrity = value.integrity or (throw "Dependency ${name} does not have an 'integrity' key");
        resolved = value.resolved or (throw "Dependency ${name} does not have a 'resolved' key");
      };
    };

    # NPM lockfiles differ depending on their version. v1 lockfiles used a
    # "dependencies" key, while v2 lockfiles use a "packages" key.
    allDependencies = lock.packages or lock.dependencies or {};
  in
    lib.mapAttrs' normalizePackage (removeCurrent allDependencies);

  # Turn each dependency into a fetchurl call. At the moment, this code does not
  # support any other type of dependency; for that, see other NPM lockfile
  # libraries like npmlock2nix.
  fetchDependencyTarball = name: dependency:
    fetchurl {
      inherit name;
      pname = name;
      version = dependency.version or (throw "Dependency ${name} does not have a 'version' key");
      url = dependency.resolved or (throw "Dependency ${name} does not have a 'resolved' key");
      hash = dependency.integrity or (throw "Dependency ${name} does not have an 'integrity' key");
    };

  # Tarballs to cache
  listDependencyTarballs = tarballs:
    (builtins.concatStringsSep "\n" (builtins.attrValues tarballs)) + "\n";

  buildPackageLock = {lockfile}: let
    packageLock = readPackageLock (builtins.readFile lockfile);
    dependencies = getDependencies packageLock;
    tarballs = writeTextFile {
      name = "tarballs";
      text = listDependencyTarballs (builtins.mapAttrs fetchDependencyTarball dependencies);
    };
  in
    stdenv.mkDerivation {
      name = packageLock.name;
      version = packageLock.version or "0.0.0";

      buildInputs = [nodejs];

      buildPhase = ''
        export HOME=$PWD/.home
        export npm_config_cache=$PWD/.npm
        mkdir -p $out
        cd $out
        cat ${tarballs} | xargs npm cache add
        npm ci
      '';

      installPhase = ''
        ln -s $out/node_modules/.bin $out/bin
      '';
    };
}

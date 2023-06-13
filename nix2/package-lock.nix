{ lib, fetchurl, writeTextFile, nodejs, stdenv, }: rec {
  # Read a package-lock.json as a Nix attrset
  readPackageLock = lockfile: builtins.fromJSON (builtins.readFile lockfile);

  getDependencies = lock:
    let
      omit = [ "" ];
      # Remove prefixes from package names listed in the lockfile, namely:
      # "node_modules/<package>"
      normalize = name: value: {
        name = (lib.last (lib.strings.splitString "node_modules/" name));
        value = value;
      };
      packages = mapAttrs normalize (removeAttrs lock.packages omit);
      dependencies =
        mapAttrs normalize (removeAttrs (lock.dependencies or { }) omit);
    in packages // dependencies;

  # Turn each dependency into a fetchurl call
  fetchDependencyTarball = name: dependency:
    fetchurl {
      url = dependency.resolved or (throw
        "Dependency ${name} does not have a 'resolved' key but has keys ${
          builtins.concatStringsSep ", " (builtins.attrNames entry)
        }");
      hash = dependency.integrity or (throw
        "Dependency ${name} does not have an 'integrity' key but has keys ${
          builtins.concatStringsSep ", " (builtins.attrNames entry)
        }");
    };

  fetchDependencyTarballs = dependencies:
    builtins.mapAttrs fetchTarball dependencies;

  # Tarballs to cache
  listDependencyTarballs = tarballs:
    (builtins.concatStringsSep "\n" (builtins.attrValues tarballs)) + "\n";

  buildPackageLock = { lockfile }:
    let
      packageLock = readPackageLock (builtins.readFile lockfile);
      dependencies = getDependencies packageLock;
      tarballs = writeTextFile {
        name = "tarballs";
        text = listDependencyTarballs (fetchDependencyTarballs dependencies);
      };
    in stdenv.mkDerivation {
      name = packageLock.name;
      version = packageLock.version or "0.0.0";

      buildInputs = [ nodejs ];

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

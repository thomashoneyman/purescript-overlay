{
  name,
  js,
  meta,
}: {
  lib,
  stdenv,
  fetchurl,
  nodejs,
  runCommand,
  buildNpmPackage,
}: version: source: let
in
  # If there is NOT a lockfile set, then we can build this as a simple bundle.
  if source.lockfile or {} == {}
  then
    stdenv.mkDerivation {
      pname = name;
      inherit version;

      src = fetchurl source;

      nativeBuildInputs = [nodejs];

      buildPhase = ''
        tar xf $src
      '';

      installPhase = ''
        PACKAGE=$out/node_modules/${name}
        mkdir -p $PACKAGE
        mv package/* $PACKAGE

        BIN=$PACKAGE/${js}
        chmod +x $BIN
        patchShebangs $BIN

        mkdir -p $out/bin
        ln -s $BIN $out/bin/${name}
      '';

      meta = meta lib;
    }
  # Otherwise, if there IS a lockfile set, then we need to include dependencies.
  else let
    packageJson = fetchurl source.lockfile;
  in
    buildNpmPackage {
      pname = name;
      inherit version;
      src = fetchurl source.tarball;

      makeCacheWritable = true;

      postPatch = ''
        cp ${packageJson} package-lock.json
      '';

      npmDepsHash = source.depsHash;

      nativeBuildInputs = [nodejs];

      # The prepack script runs the build script, but (so far) all derivations
      # are pre-built.
      npmPackFlags = ["--ignore-scripts"];
      dontNpmBuild = true;

      meta = meta lib;
    }

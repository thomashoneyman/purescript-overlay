{
  name,
  js,
  meta,
}: {
  lib,
  stdenv,
  fetchurl,
  nodejs,
  python3,
  darwin,
  nodePackages,
  buildNpmPackage,
}: version: source: let
in
  # If there is NOT a lockfile set, then we can build this as a simple bundle.
  # We know that if it has the 'fetchurl' shape, ie. { url, hash }
  if source.url or {} != {}
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
    packageJson =
      if (builtins.hasAttr "lockfile" source)
      then fetchurl source.lockfile
      else "${./. + ("/" + source.path)}";
  in
    buildNpmPackage {
      pname = name;
      inherit version;
      src = fetchurl source.tarball;

      postPatch = ''
        cp ${packageJson} package-lock.json
      '';

      npmDepsHash = source.depsHash;

      nativeBuildInputs = [nodejs] ++ lib.optionals (name == "spago") ([nodePackages.node-gyp python3] ++ lib.optionals stdenv.isDarwin [darwin.cctools]);

      # The prepack script runs the build script, but (so far) all derivations
      # are pre-built.
      npmPackFlags = ["--ignore-scripts"];
      dontNpmBuild = true;

      npmInstallFlags = ["--loglevel=verbose"];

      meta = meta lib;
    }

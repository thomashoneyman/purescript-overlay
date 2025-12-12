{
  name,
  js,
  meta,
}:
{
  lib,
  stdenv,
  fetchurl,
  nodejs,
  nodejs_20,
  python3,
  darwin,
  nodePackages,
  buildNpmPackage,
}:
version: source:
let
  # Check if this is a simple tarball (no npm dependencies needed)
  isSimpleTarball = source.url or { } != { };

  commonMeta = meta lib // {
    mainProgram = name;
  };
in
# Simple tarball without dependencies - just extract and link
if isSimpleTarball then
  stdenv.mkDerivation {
    pname = name;
    inherit version;

    src = fetchurl source;

    nativeBuildInputs = [ nodejs ];

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

    meta = commonMeta;
  }
# Package with npm dependencies - use buildNpmPackage
else
  let
    # Use Node.js 20 for spago to avoid better-sqlite3 build issues with Node 22+
    # Same approach as registry-dev: https://github.com/purescript/registry-dev/blob/87c5900ff6fb7090cf2b085b7eb3f75371560522/nix/overlay.nix#L152
    isSpago = name == "spago";
    selectedNodejs = if isSpago then nodejs_20 else nodejs;

    packageJson =
      if (builtins.hasAttr "lockfile" source) then
        fetchurl source.lockfile
      else
        "${./. + ("/" + source.path)}";

    baseNativeBuildInputs = [ selectedNodejs ];

    # Spago needs additional native build tools for better-sqlite3
    spagoNativeBuildInputs = [
      nodePackages.node-gyp
      python3
    ]
    ++ lib.optionals stdenv.isDarwin [ darwin.cctools ];

    allNativeBuildInputs = baseNativeBuildInputs ++ lib.optionals isSpago spagoNativeBuildInputs;
  in
  (buildNpmPackage.override { nodejs = selectedNodejs; }) {
    pname = name;
    inherit version;
    src = fetchurl source.tarball;

    postPatch = ''
      cp ${packageJson} package-lock.json
    '';

    npmDepsHash = source.depsHash;

    nativeBuildInputs = allNativeBuildInputs;

    # The prepack script runs the build script, but (so far) all derivations
    # are pre-built.
    npmPackFlags = [ "--ignore-scripts" ];
    dontNpmBuild = true;

    npmInstallFlags = [ "--loglevel=verbose" ] ++ lib.optionals isSpago [ "--omit=optional" ];

    meta = commonMeta;
  }

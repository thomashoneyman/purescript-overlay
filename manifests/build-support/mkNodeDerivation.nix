# Builds Node.js-based PureScript tools from npm tarballs.
#
# Supports three source formats:
# 1. Simple tarball: { url, hash } - just extract and link the binary
# 2. NPM package with deps: { tarball, depsHash, lockfile? } - use buildNpmPackage
# 3. Single JS file: { url, hash } with singleJsFile=true - download and wrap
{
  stdenv,
  fetchurl,
  nodejs,
  buildNpmPackage,
  writeText,
}:
{
  pname,
  meta,
  # Path to the JS binary inside the package
  binPath,
  # For single JS file downloads (like purescript-language-server)
  singleJsFile ? false,
  # Function: version -> nodejs derivation
  selectNodejs ? _: nodejs,
  # Extra native build inputs (e.g., for native modules)
  extraNativeBuildInputs ? [ ],
  # Function: { version, nodejs, binPath } -> string, extra postInstall commands
  postInstall ? _: "",
}:
version: source:
let
  isSimpleTarball = source ? url && !(source ? tarball);
  isNpmPackage = source ? tarball && source ? depsHash;

  selectedNodejs = selectNodejs version;

  commonMeta = meta // {
    mainProgram = pname;
  };
in
if singleJsFile then
  # Single JS file download (purescript-language-server style)
  stdenv.mkDerivation {
    inherit pname version;

    src = fetchurl source;

    nativeBuildInputs = [ nodejs ];

    dontUnpack = true;

    packageJSON = writeText "package.json" ''
      {
        "name": "${pname}",
        "version": "${version}"
      }
    '';

    installPhase = ''
      PACKAGE=$out/node_modules/${pname}
      mkdir -p $PACKAGE
      cp $src $PACKAGE/${pname}.js
      cp $packageJSON $PACKAGE/package.json

      BIN=$PACKAGE/${pname}.js
      chmod +x $BIN
      patchShebangs $BIN

      mkdir -p $out/bin
      ln -s $BIN $out/bin/${pname}
    '';

    meta = commonMeta;
  }
else if isSimpleTarball then
  # Simple tarball without npm dependencies - just extract and link
  stdenv.mkDerivation {
    inherit pname version;

    src = fetchurl source;

    nativeBuildInputs = [ nodejs ];

    buildPhase = ''
      tar xf $src
    '';

    installPhase = ''
      PACKAGE=$out/node_modules/${pname}
      mkdir -p $PACKAGE
      mv package/* $PACKAGE

      BIN=$PACKAGE/${binPath}
      chmod +x $BIN
      patchShebangs $BIN

      mkdir -p $out/bin
      ln -s $BIN $out/bin/${pname}
    '';

    meta = commonMeta;
  }
else if isNpmPackage then
  # NPM package with dependencies - use buildNpmPackage
  let
    packageJson =
      if source ? lockfile then
        fetchurl source.lockfile
      else
        throw "NPM package ${pname} ${version} requires a lockfile";
  in
  (buildNpmPackage.override { nodejs = selectedNodejs; }) {
    inherit pname version;

    src = fetchurl source.tarball;

    postPatch = ''
      cp ${packageJson} package-lock.json
    '';

    npmDepsHash = source.depsHash;

    nativeBuildInputs = [ selectedNodejs ] ++ extraNativeBuildInputs;

    # The prepack script runs the build script, but all derivations are pre-built
    npmPackFlags = [ "--ignore-scripts" ];
    dontNpmBuild = true;

    npmInstallFlags = [
      "--logs-max=0"
      "--omit=optional"
    ];

    postInstall = postInstall {
      inherit version binPath;
      nodejs = selectedNodejs;
    };

    meta = commonMeta;
  }
else
  throw "Unknown source format for ${pname} ${version}: expected url, tarball+depsHash, or singleJsFile"

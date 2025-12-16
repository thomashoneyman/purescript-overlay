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
  nodejs_20 ? null,
  python3,
  darwin,
  nodePackages,
  buildNpmPackage,
  installShellFiles,
  makeWrapper,
}:
version: source:
let
  # Check if this is a simple tarball (no npm dependencies needed)
  isSimpleTarball = source.url or { } != { };

  commonMeta = meta lib // {
    mainProgram = name;
  };

  isSpago = name == "spago";

  # Legacy spago versions (< 0.93.45) require Node.js 20 due to better-sqlite3
  # build issues with Node 22+. Spago 0.93.45+ works with the standard nodejs.
  # See: https://github.com/purescript/registry-dev/blob/87c5900ff6fb7090cf2b085b7eb3f75371560522/nix/overlay.nix#L152
  isLegacySpago = isSpago && builtins.compareVersions version "0.93.45" < 0;

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
    selectedNodejs = if isLegacySpago then nodejs_20 else nodejs;

    packageJson =
      if builtins.hasAttr "lockfile" source then
        fetchurl source.lockfile
      else
        "${./. + ("/" + source.path)}";

    # Spago needs native build tools for better-sqlite3
    spagoNativeBuildInputs = [
      nodePackages.node-gyp
      python3
      makeWrapper
      installShellFiles
    ]
    ++ lib.optionals stdenv.isDarwin [ darwin.cctools ];
  in
  (buildNpmPackage.override { nodejs = selectedNodejs; }) {
    pname = name;
    inherit version;

    src = fetchurl source.tarball;

    postPatch = ''
      cp ${packageJson} package-lock.json
    '';

    npmDepsHash = source.depsHash;

    nativeBuildInputs = [ selectedNodejs ] ++ lib.optionals isSpago spagoNativeBuildInputs;

    # The prepack script runs the build script, but (so far) all derivations
    # are pre-built.
    npmPackFlags = [ "--ignore-scripts" ];
    dontNpmBuild = true;

    npmInstallFlags = [
      "--logs-max=0"
      "--omit=optional"
    ];

    # For spago: Replace the symlink with a wrapper that invokes node directly
    # with the script renamed via a symlink. This fixes spago displaying
    # "Usage: bundle.js" instead of "Usage: spago" (Node.js uses the script
    # path for the program name). Also generate shell completions.
    postInstall = lib.optionalString isSpago ''
      rm $out/bin/${name}
      ln -s $out/lib/node_modules/${name}/${js} $out/lib/node_modules/${name}/bin/${name}
      makeWrapper ${selectedNodejs}/bin/node $out/bin/${name} \
        --add-flags $out/lib/node_modules/${name}/bin/${name}

      installShellCompletion --cmd ${name} \
        --bash <($out/bin/${name} --bash-completion-script $out/bin/${name}) \
        --zsh <($out/bin/${name} --zsh-completion-script $out/bin/${name}) \
        --fish <($out/bin/${name} --fish-completion-script $out/bin/${name})
    '';

    meta = commonMeta;
  }

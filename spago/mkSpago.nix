{
  stdenv,
  lib,
  writeText,
  esbuild,
  nodejs,
  writeShellScriptBin,
  # via spago-nix overlay
  spago-lock,
  spago-npm-dependencies,
  # specific version provided via flake
  purs,
  # required additional arguments
  version,
  rev,
}: let
  bundle = stdenv.mkDerivation rec {
    pname = "spago";
    inherit version;

    nativeBuildInputs = [purs esbuild nodejs];

    repo = builtins.fetchGit {
      url = "https://github.com/purescript/spago.git";
      rev = rev;
      allRefs = true;
    };

    src = repo + "/spaghetto";

    buildInfo = writeText "BuildInfo.purs" ''
      module Spago.Generated.BuildInfo where

      buildInfo =
        { packages: []
        , pursVersion: "${purs.version}"
        , spagoVersion: "${version}"
        }
    '';

    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Main";
      main();
    '';

    buildPhase = let
      npmDependencies = spago-npm-dependencies {inherit src;};
      workspaces = spago-lock {inherit src;};
    in ''
      # Make sure the node_modules folder is available
      ln -s ${npmDependencies}/js/node_modules .

      set -f
      ${purs}/bin/purs compile $src/bin/src/**/*.purs ${workspaces.spago-bin.dependencies.globs} ${buildInfo}
      set +f

      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js \
        --bundle \
        --minify \
        --outfile=bundle.js \
        --platform=node
    '';

    installPhase = ''
      mkdir $out
      cp bundle.js $out
    '';

    meta.description = "PureScript package manager and build tool";
  };
in
  writeShellScriptBin "spago" ''
    ${nodejs}/bin/node ${bundle}/bundle.js "$@"
  ''

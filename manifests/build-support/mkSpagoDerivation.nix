{
  stdenv,
  lib,
  writeText,
  esbuild,
  nodejs,
  writeShellScriptBin,
  # via overlay
  purix,
  purs-bin,
}: {
  version,
  manifest,
}: let
  purs = purs-bin.purs-0_15_9; # TODO: Discover this from the lockfile or manifest?

  repo = builtins.fetchGit {
    url = "https://github.com/purescript/spago.git";
    rev = manifest.rev;
    allRefs = true;
  };

  src = repo + "/spaghetto";

  buildInfo = writeText "BuildInfo.purs" ''
    module Spago.Generated.BuildInfo where

    buildInfo :: { packages :: Array { name :: String, version :: String }, pursVersion :: String, spagoVersion :: String }
    buildInfo =
      { packages: [{ name: "spago", version: "${version}"}, { name: "spago-bin", version: "${version}"}, { name: "spago-core", version: "${version}"}]
      , pursVersion: "${purs.version}"
      , spagoVersion: "${version}"
      }
  '';

  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output/Main";
    main();
  '';
in
  stdenv.mkDerivation {
    pname = "spago";
    inherit version src;

    nativeBuildInputs = [purs esbuild];
    buildInputs = [nodejs];

    buildPhase = let
      npmDependencies = purix.buildPackageLock {src = src;};
      workspaces = purix.buildSpagoLock {
        src = src;
        lockfile = src + "/spago.lock";
      };
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
      mkdir -p $out/bin
      cp bundle.js $out/bundle.js
      cat ${buildInfo} > $out/BuildInfo.purs
      echo '#!/usr/bin/env sh' > $out/bin/spago
      echo 'exec ${nodejs}/bin/node '"$out/bundle.js"' "$@"' >> $out/bin/spago
      chmod +x $out/bin/spago
    '';

    meta = with lib; {
      description = "PureScript package manager and build tool";
      homepage = "https://github.com/purescript/spago";
      license = licenses.bsd3;
    };
  }

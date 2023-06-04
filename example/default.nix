{
  stdenv,
  writeText,
  nodejs,
  # from purescript-nix
  buildPackageLock,
  buildSpagoLock,
  purs,
  esbuild,
}: let
  npmDependencies = buildPackageLock {src = ./.;};
  workspaces = buildSpagoLock {
    purs = purs.purs-unstable;
    src = ./.;
  };
  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output/App.Main";
    main();
  '';
in
  stdenv.mkDerivation rec {
    name = "my-app";
    src = ./.;
    nativeBuildInputs = [purs.purs-unstable esbuild];
    buildPhase = ''
      ln -s ${npmDependencies}/js/node_modules .
      set -f
      purs compile $src/my-app/**/*.purs ${workspaces.${name}.dependencies.globs}
      set +f
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js \
        --bundle \
        --minify \
        --outfile=${name}.js \
        --platform=node
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp ${name}.js $out/${name}.js
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
      cp ${name}.js $out
    '';
  }

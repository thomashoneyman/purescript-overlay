{
  stdenv,
  writeText,
  nodejs,
  esbuild,
  # from purix
  purix,
  purs,
  purs-backend-es,
}: let
  npmDependencies = purix.lib.buildPackageLock {src = ./.;};
  workspaces = purix.lib.workspaces {
    src = ./.;
    lockfile = ./. + "/spago.lock";
  };
  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output-es/Bin.Main";
    main();
  '';
in
  stdenv.mkDerivation rec {
    name = "bin";
    src = ./.;
    nativeBuildInputs = [purs purs-backend-es esbuild];
    buildPhase = ''
      ln -s ${npmDependencies}/js/node_modules .
      set -f
      purs compile $src/${name}/**/*.purs ${workspaces.${name}.dependencies.globs} --codegen corefn
      purs-backend-es build
      set +f
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js \
        --bundle \
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

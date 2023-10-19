{
  stdenv,
  writeText,
  nodejs,
  esbuild,
  slimlock,
  prefetch-npm-deps,
  makeWrapper,
  # from purix
  purix,
  purs-backend-es,
  purs-tidy,
}: let
  npmDependencies = slimlock.buildPackageLock {src = ./.;};
  packages = purix.buildSpagoLock {
    src = ./.;
    corefn = true;
  };
  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output-es/Bin.Main";
    main();
  '';
in
  stdenv.mkDerivation rec {
    name = "bin";
    src = ./.;
    nativeBuildInputs = [purs-backend-es purs-tidy esbuild makeWrapper];

    buildPhase = ''
      ln -s ${npmDependencies}/js/node_modules .
      cp -r ${packages.${name}}/output .
      set -f
      echo "Optimizing..."
      purs-backend-es build
      set +f
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js \
        --bundle \
        --outfile=${name}.js \
        --platform=node
    '';

    checkPhase = ''
      purs-tidy check bin lib
      eslint lib
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp ${name}.js $out/${name}.js
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
      cp ${name}.js $out
    '';

    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${lib.makeBinPath [prefetch-npm-deps]}
    '';
  }

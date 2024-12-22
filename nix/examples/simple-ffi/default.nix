{
  stdenv,
  writeText,
  esbuild,
  purix,
  purs-bin,
}: let
  locked = purix.buildSpagoLock {src = ./.;};
  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output/Main";
    main();
  '';
in
  stdenv.mkDerivation {
    name = "simple-ffi";
    src = ./.;
    nativeBuildInputs = [purs-bin.purs-0_15_9 esbuild];
    buildPhase = ''
      echo "Linking ..."
      ln -s ${locked.npmDependencies}/js/node_modules .
      cp -r ${locked.jsArtifacts.simple-ffi}/output .
      cp ${entrypoint} entrypoint.js
      cat entrypoint.js
      esbuild entrypoint.js \
        --bundle \
        --outfile=app.js \
        --platform=node
    '';
    installPhase = ''
      mkdir $out
      cp app.js $out
    '';
  }

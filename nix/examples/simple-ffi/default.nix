{
  stdenv,
  writeText,
  esbuild,
  slimlock,
  purix,
  purs-bin,
}: let
  npmDependencies = slimlock.buildPackageLock {src = ./.;} + "/js/node_modules";
  locked = purix.buildSpagoLock {
    src = ./.;
    inherit npmDependencies;
  };
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
      ln -s ${npmDependencies} .
      cp -r ${locked.simple-ffi}/output .
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

{
  stdenv,
  writeText,
  esbuild,
  slimlock,
  purix,
  purs-bin,
}: let
  packageLock = slimlock.buildPackageLock {src = ./.;};
  spagoLock = purix.buildSpagoLock {src = ./.;};
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
      ln -s ${packageLock}/js/node_modules .
      cp -r ${spagoLock.simple-ffi}/output .
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

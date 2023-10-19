{
  name,
  js,
  meta,
}: {
  lib,
  stdenv,
  fetchurl,
  nodejs,
  runCommand,
  slimlock,
  pkgs,
}: version: source: let
  node_modules =
    if source.lockfile or {} != {}
    then
      (slimlock.buildPackageLock {
        src = runCommand "package-lock-dir" {} ''
          mkdir -p $out
          cp ${./. + ("/" + source.lockfile)} $out/package-lock.json
        '';
        omit = ["dev" "peer"];
      })
      .overrideAttrs (final: prev: {
        nativeBuildInputs =
          (prev.nativeBuildInputs or [])
          ++ [pkgs.python3]
          ++ (
            if stdenv.isDarwin
            then [pkgs.libtool]
            else []
          );
      })
    else {};
in
  stdenv.mkDerivation {
    pname = name;

    inherit version;

    # Source is either { url, hash } or { tarball: { url, hash } }
    src =
      if source.url or {} != {}
      then fetchurl source
      else fetchurl source.tarball;

    nativeBuildInputs = [nodejs];

    buildPhase = ''
      tar xf $src
    '';

    installPhase = ''
      mkdir -p $out/node_modules

      ${
        if node_modules != {}
        then ''
          cp -r ${node_modules}/js/node_modules $out
        ''
        else ''
          echo "No package-lock.json found, not including node_modules."
        ''
      }

      PACKAGE=$out/node_modules/${name}
      mkdir -p $PACKAGE
      mv package/* $PACKAGE

      BIN=$PACKAGE/${js}
      chmod +x $BIN
      patchShebangs $BIN

      mkdir -p $out/bin
      ln -s $BIN $out/bin/${name}
    '';

    meta = meta lib;
  }

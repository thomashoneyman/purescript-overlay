{
  stdenv,
  purix,
}: let
  packageLock = purix.buildPackageLock {src = ./.;};
in
  stdenv.mkDerivation {
    name = "npm-workspaces";
    src = ./.;
    installPhase = ''
      cp -r ${packageLock}/js/node_modules $out
    '';
  }

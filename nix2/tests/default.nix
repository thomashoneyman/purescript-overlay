{
  stdenv,
  callPackage,
  system,
}: let
  testUtils = callPackage ./utils.nix {};
  args = {inherit (testUtils) runTests;};
in
  builtins.attrValues {
    testFromYAML = callPackage ./unit/from-yaml.nix args;
    testPackageLock = callPackage ./unit/package-lock.nix args;
  }

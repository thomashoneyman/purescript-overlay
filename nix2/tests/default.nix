{
  stdenv,
  callPackage,
  system,
}: let
  testUtils = callPackage ./utils.nix {};
  args = {inherit (testUtils) runTests;};
in
  builtins.attrValues {
    testFromYAML = callPackage ./unit/fromYAML.nix args;
  }

# This file collects the unit tests from this directory. Each top-level
# attribute should point to a derivation that runs the tests for a specific
# feature via runTests
{
  lib,
  callPackage,
}:
lib.remove [] (builtins.attrValues {
  testFromYAML = callPackage ./from-yaml {};
  testPackageLock = callPackage ./package-lock {};
  testSpagoLock = callPackage ./spago-lock {};
})

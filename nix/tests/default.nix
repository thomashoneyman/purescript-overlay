# This file collects the unit tests from this directory. Each top-level
# attribute should point to a derivation that runs the tests for a specific
# feature via runTests.
#
# Runnable with nix eval .#lib
{
  lib,
  callPackage,
}:
lib.remove [] (builtins.attrValues {
  testFromYAML = callPackage ./from-yaml {};
  testSpagoLock = callPackage ./build-spago-lock {};
  testSpagoConfig = callPackage ./spago-config {};
})

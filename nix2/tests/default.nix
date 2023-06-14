{
  lib,
  callPackage,
}:
lib.remove [] (builtins.attrValues {
  testFromYAML = callPackage ./from-yaml {};
  testPackageLock = callPackage ./package-lock {};
  testSpagoLock = callPackage ./spago-lock {};
})

{
  callPackage,
  runTests,
}: let
  buildPackageLock = callPackage ../../buildPackageLock.nix {};
in
  runTests {
    testEmpty = {
      expr = "";
      expected = "";
    };
  }

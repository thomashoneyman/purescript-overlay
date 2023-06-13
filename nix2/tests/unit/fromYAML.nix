{
  callPackage,
  runTests,
}: let
  fromYAML = callPackage ../../fromYAML.nix {};
  exception = {
    success = false;
    value = false;
  };
in
  runTests {
    testEmpty = {
      expr = (builtins.tryEval (fromYAML " ")).success;
      expected = false;
    };
  }

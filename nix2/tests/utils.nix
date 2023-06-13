{lib}: rec {
  # Reads a given file (either drv, path or string) and returns its sha256 hash
  hashFile = file: builtins.hashString "sha256" (builtins.readFile file);

  # Takes an attrset of tests and throws an error if any of them fail. A test
  # is an attrset where keys are test names and values are { expr, expected }
  runTests = tests: let
    failures = lib.debug.runTests tests;
    message =
      ''
        Tests failed:
      ''
      + lib.concatMapStringsSep "\n" (fail: ''
        FAIL: ${fail.name}
          expected: ${builtins.toJSON fail.expected}
               got: ${builtins.toJSON fail.result}
      '')
      failures;
  in
    if builtins.length failures == 0
    then "All succeeded"
    else builtins.throw message;

  # Nix's runTests by default will run top-level tests but ignores nested
  # attributes. This function takes an attrset of tests and runs all of them.
  runTestSuite = tests: builtins.attrValues (builtins.mapAttrs (name: nested: runTests nested) tests);
}

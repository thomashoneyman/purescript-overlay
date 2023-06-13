{lib}: {
  # Reads a given file (either drv, path or string) and returns its sha256 hash
  hashFile = file: builtins.hashString "sha256" (builtins.readFile file);

  # Takes an attrset of tests and throws an error if any of them fail. A test
  # is an attrset where keys are test names and values are { expr, expected }
  runTests = tests: let
    testCount = builtins.length (builtins.attrNames tests);
    failures = lib.debug.runTests tests;
    failureCount = builtins.length failures;
    message =
      ''
        ${builtins.toString failureCount} out of ${builtins.toString testCount} tests failed:
      ''
      + lib.concatMapStringsSep "\n" (fail: ''
        ${fail.name}
          expected: ${builtins.toJSON fail.expected}
               got: ${builtins.toJSON fail.result}
      '')
      failures;
  in
    if builtins.length failures == 0
    then "All succeeded"
    else builtins.throw message;
}

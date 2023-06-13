{callPackage}: let
  utils = callPackage ../utils.nix {};
  lock = callPackage ../../package-lock.nix {};
in
  utils.runTests {
    testSimple = {
      expr = lock.getDependencies (lock.readPackageLock ./simple.json);
      expected = {
        leftpad = {
          version = "0.0.1";
          integrity = "sha512-kBAuxBQJlJ85LDc+SnGSX6gWJnJR9Qk4lbgXmz/qPfCOCieCk7BgoN3YvzoNr5BUjqxQDOQxawJJvXXd6c+6Mg==";
          resolved = "https://registry.npmjs.org/leftpad/-/leftpad-0.0.1.tgz";
        };
      };
    };
  }

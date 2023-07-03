{callPackage}: let
  utils = callPackage ../utils.nix {};
  buildPackageLock = callPackage ../../build-package-lock.nix {};
in
  utils.runTests {
    testSimpleDependencies = {
      expr = buildPackageLock.getDependencies (buildPackageLock.readPackageLock ./simple.json);
      expected = {
        "node_modules/leftpad" = {
          version = "0.0.1";
          integrity = "sha512-kBAuxBQJlJ85LDc+SnGSX6gWJnJR9Qk4lbgXmz/qPfCOCieCk7BgoN3YvzoNr5BUjqxQDOQxawJJvXXd6c+6Mg==";
          resolved = "https://registry.npmjs.org/leftpad/-/leftpad-0.0.1.tgz";
        };
      };
    };
  }

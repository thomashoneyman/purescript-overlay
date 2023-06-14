{callPackage}: let
  utils = callPackage ../utils.nix {};
  lock = callPackage ../../package-lock.nix {};
in
  utils.runTests {
    testSimpleDependencies = {
      expr = lock.getDependencies (lock.readPackageLock ./simple.json);
      expected = {
        leftpad = {
          version = "0.0.1";
          integrity = "sha512-kBAuxBQJlJ85LDc+SnGSX6gWJnJR9Qk4lbgXmz/qPfCOCieCk7BgoN3YvzoNr5BUjqxQDOQxawJJvXXd6c+6Mg==";
          resolved = "https://registry.npmjs.org/leftpad/-/leftpad-0.0.1.tgz";
        };
      };
    };

    testSimpleTarballs = {
      expr = lock.listDependencyTarballs (builtins.mapAttrs lock.fetchDependencyTarball (lock.getDependencies (lock.readPackageLock ./simple.json)));
      expected = ''
        /nix/store/f33qz45a96dzl1dh1wzfy705cm5bswq9-leftpad-0.0.1
      '';
    };
  }

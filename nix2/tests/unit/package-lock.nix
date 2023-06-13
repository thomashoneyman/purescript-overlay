{ callPackage, writeTextFile, runTests, }:
let
  lock = callPackage ../../package-lock.nix { };

  fixtures = {
    simple = writeTextFile {
      name = "package-lock.json";
      text = ''
        {
          "name": "simple",
          "version": "1.0.0",
          "lockfileVersion": 3,
          "requires": true,
          "packages": {
            "": {
              "name": "simple",
              "version": "1.0.0",
              "dependencies": {
                "leftpad": "0.0.1"
              }
            },
            "node_modules/leftpad": {
              "version": "0.0.1",
              "resolved": "https://registry.npmjs.org/leftpad/-/leftpad-0.0.1.tgz",
              "integrity": "sha512-kBAuxBQJlJ85LDc+SnGSX6gWJnJR9Qk4lbgXmz/qPfCOCieCk7BgoN3YvzoNr5BUjqxQDOQxawJJvXXd6c+6Mg==",
              "deprecated": "Use the built-in String.padStart function instead"
            }
          }
        }
      '';
    };
  };
in runTests {
  testSimple = {
    expr = lock.getDependencies (lock.readPackageLock fixtures.simple);
    expected = "";
  };
}

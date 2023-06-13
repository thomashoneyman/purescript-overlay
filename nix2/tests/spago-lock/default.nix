{callPackage}: let
  utils = callPackage ../utils.nix {};
  fromYAML = callPackage ../../from-yaml.nix {};
  lock = callPackage ../../spago-lock.nix {inherit fromYAML;};
in
  utils.runTests {
    testSimple = {
      expr = let
        locked = lock.readSpagoLock ./simple.lock;
      in
        builtins.attrNames (locked.packages // locked.workspace.packages);
      expected = ["basic" "console" "effect" "prelude"];
    };
  }

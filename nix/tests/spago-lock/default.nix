{ callPackage }:
let
  utils = callPackage ../utils.nix { };
  fromYAML = callPackage ../../from-yaml.nix { };
  lock = callPackage ../../spago-lock.nix { inherit fromYAML; };
in utils.runTests {
  testSimpleLock = {
    expr = let locked = lock.readSpagoLock ./simple.lock;
    in builtins.attrNames (locked.packages // locked.workspace.packages);
    expected = [ "basic" "console" "effect" "prelude" ];
  };

  testSimpleconfig = {
    expr = lock.readSpagoConfig ./simple.yaml;
    expected = {
      package = {
        name = "basic";
        dependencies = [ "console" "effect" "prelude" ];
        test = {
          dependencies = [ ];
          main = "Test.Main";
        };
      };
      workspace = {
        extra_packages = { };
        package_set = { registry = "25.2.1"; };
      };
    };
  };

  testWorkspaceLock = {
    expr = let locked = lock.readSpagoLock ./workspaces.lock;
    in builtins.attrNames (locked.workspace.packages);
    expected = [ "bin" "lib" ];
  };
}

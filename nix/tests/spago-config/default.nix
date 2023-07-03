{ callPackage }:
let
  utils = callPackage ../utils.nix { };
  fromYAML = callPackage ../../from-yaml.nix { };
  config = callPackage ../../spago-config.nix { inherit fromYAML; };
in utils.runTests {
  testSimpleconfig = {
    expr = config.fromSpagoConfig ''
      workspace:
        lock: true
        package_set:
          registry: 24.4.0
        extra_packages:
          registry-lib:
            git: https://github.com/purescript/registry-dev.git
            ref: master
            subdir: lib
    '';
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
}

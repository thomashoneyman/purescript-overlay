{ callPackage }:
let
  utils = callPackage ../utils.nix { };
  fromYAML = callPackage ../../from-yaml.nix { };
in
utils.runTests {
  testSimpleconfig = {
    expr = fromYAML ''
      workspace:
        lock: true
        package_set:
          registry: 24.4.0
        extra_packages: {}
    '';
    expected = {
      workspace = {
        lock = true;
        extra_packages = { };
        package_set = {
          registry = "24.4.0";
        };
      };
    };
  };
}

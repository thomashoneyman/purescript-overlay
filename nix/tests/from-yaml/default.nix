{callPackage}: let
  utils = callPackage ../utils.nix {};
  fromYAML = callPackage ../../from-yaml.nix {};
  tryYAML = input: builtins.tryEval (fromYAML input);
  shouldParse = {
    input,
    output,
  }: {
    expr = (tryYAML input).value;
    expected = output;
  };
  shouldNotParse = input: {
    expr = (tryYAML input).success;
    expected = false;
  };
in
  utils.runTests {
    testEmpty = shouldNotParse "\n";

    testEmptyList = shouldParse {
      input = ''
        dependencies: []
      '';
      output = {
        dependencies = [];
      };
    };

    testEmptyObject = shouldParse {
      input = ''
        extra_packages: {}
      '';
      output = {
        extra_packages = {};
      };
    };
  }

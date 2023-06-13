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
    testEmpty = shouldNotParse " ";

    testLock = shouldParse {
      input = ''
        exists:
          type: registry
          version: 6.0.0
          integrity: sha256-A0JQHpTfo1dNOj9U5/Fd3xndlRSE0g2IQWOGor2yXn8=
          dependencies:
            - unsafe-coerce
      '';
      output = {
        exists = {
          type = "registry";
          version = "6.0.0";
          integrity = "sha256-A0JQHpTfo1dNOj9U5/Fd3xndlRSE0g2IQWOGor2yXn8=";
          dependencies = ["unsafe-coerce"];
        };
      };
    };
  }

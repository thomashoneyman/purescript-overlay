# See: https://github.com/purescript/spago/blob/master/spaghetto/core/src/Config.purs
{ fromYAML, lib }: {
  fromSpagoConfig = text:
    let
      yaml = fromYAML text;

      workspace = yaml.workspace or (throw ''
        spago.yaml is missing a 'workspace' top-level field: ${text}
      '');

      packages = yaml.package or (throw ''
        spago.yaml is missing a 'package' top-level field: ${text}
      '');

      config = { inherit workspace; };

    in config;
}


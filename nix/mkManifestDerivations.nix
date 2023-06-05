{
  system,
  callPackage,
  bin,
}: let
  builder =
    if bin == "purs"
    then callPackage ./mkPursDerivation.nix {}
    else if bin == "spago"
    then callPackage ./mkSpagoDerivation.nix {}
    else throw "Unsupported bin name ${bin}";

  entries = builtins.fromJSON (builtins.readFile (../manifests + "/${bin}.json"));

  # The PureScript file is broken down by system, as PureScript offers
  # different binaries for different platforms and not all versions have
  # support for all platforms.
  versionNames = builtins.attrNames (
    if bin == "purs"
    then entries.${system}
    else entries
  );

  versions = builtins.foldl' (
    acc: version: let
      manifest =
        if bin == "purs"
        then entries.${system}.${version}
        else entries.${version};
      args = {inherit version manifest;};
      name = "${bin}-${builtins.replaceStrings ["."] ["_"] version}";
    in
      acc // {${name} = builder args;}
  ) {}
  versionNames;
in
  versions

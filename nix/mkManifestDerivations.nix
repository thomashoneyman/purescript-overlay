{
  bin,
  callPackage,
}: let
  builder =
    if bin == "purs"
    then callPackage ./mkPursDerivation.nix {}
    else if bin == "spago"
    then callPackage ./mkSpagoDerivation.nix {}
    else throw "Unsupported bin name ${bin}";

  entries = builtins.fromJSON (builtins.readFile (../manifests + "/${bin}.json"));

  versions = builtins.foldl' (
    acc: version: let
      manifest = entries.${version};
      args = {inherit version manifest;};
      name = "${bin}-${builtins.replaceStrings ["."] ["_"] version}";
    in
      acc // {${name} = builder args;}
  ) {} (builtins.attrNames entries);
in
  versions

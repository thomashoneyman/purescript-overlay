{
  lib,
  callPackage,
  parseSemVer,
}: let
  buildFieldName = parsed: let
    prefix = "spago-";
    version = parsed.major + "_" + parsed.minor + "_" + parsed.patch;
  in
    prefix
    + version
    + (
      if parsed.preRelease != ""
      then "-" + parsed.preRelease
      else ""
    );

  parseSpagoFiles = dir: let
    reserved = ["spago.nix" "mkSpagoDerivation.nix"];
    files = builtins.filter (name: !(lib.elem name reserved)) (builtins.attrNames (builtins.readDir dir));
    versions = map (file: builtins.baseNameOf (lib.removeSuffix ".nix" file)) files;
    parsedVersions = map parseSemVer versions;

    fieldNames = map buildFieldName parsedVersions;
    fieldValues = map (version: callPackage ./mkSpagoDerivation.nix (import "${dir}/${version}.nix")) versions;

    attrset = lib.genAttrs fieldNames fieldValues;
  in
    builtins.trace attrset attrset;
in
  parseSpagoFiles ./.

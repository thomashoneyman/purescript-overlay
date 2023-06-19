# This derivation exposes all the tools described by the manifests in this
# directory.
{
  stdenv,
  lib,
  system,
  callPackage,
}: let
  mkPursDerivation = callPackage ./build-support/mkPursDerivation.nix {};
  mkSpagoDerivation = callPackage ./build-support/mkSpagoDerivation.nix {};
  mkLegacySpagoDerivation = callPackage ./build-support/mkLegacySpagoDerivation.nix {};

  # The purs manifest uses fetchurl to fetch tarballs. We have to accommodate
  # missing systems.
  purs-bin = let
    entries = builtins.fromJSON (builtins.readFile ./purs.json);
  in
    builtins.foldl'
    (
      acc: version: let
        name = "purs-${builtins.replaceStrings ["."] ["_"] version}";
        entry = entries.${version}.${system} or {};
      in
        if entry != {}
        then acc // {${name} = mkPursDerivation ({inherit version;} // entry);}
        else acc
    ) {}
    (builtins.attrNames entries);

  # Spago versions prior to 0.90.0 use fetchurl for Haskell tarballs, but after
  # that we build with PureScript and Node.
  spago-bin = let
    entries = builtins.fromJSON (builtins.readFile ./spago.json);
  in
    builtins.foldl'
    (
      acc: version: let
        name = "spago-${builtins.replaceStrings ["."] ["_"] version}";
        legacyEntry = entries.${version}.${system} or {};
      in
        if legacyEntry != {}
        then acc // {${name} = mkLegacySpagoDerivation ({inherit version;} // legacyEntry);}
        else acc // {${name} = mkSpagoDerivation ({inherit version;} // entries.${version});}
    ) {}
    (builtins.attrNames entries);

  all = {
    inherit purs-bin spago-bin;
  };

  # The 'named.json' file records what packages should be mapped to the default
  # package names. This is the latest stable or unstable version for each package.
  named = let
    entries = builtins.fromJSON (builtins.readFile ./named.json);
  in
    builtins.foldl'
    (
      acc: key: let
        name = builtins.replaceStrings ["-stable"] [""] key;
        value = entries.${key};
        group = "${builtins.replaceStrings ["-stable" "-unstable"] ["" ""] key}-bin";
        available = builtins.attrNames all.${group};
      in
        acc
        // {
          ${name} =
            all.${group}.${value}
            or (stdenv.mkDerivation rec {
              name = builtins.replaceStrings ["-stable" "-unstable"] ["" ""] key;
              unsupported = true;
              buildCommand = ''
                echo "No binary available for ${key} ${value} on system ${system}"
                echo "Available versions: ${builtins.concatStringsSep " " available}"
                echo "Please specify a version from the list above."
              '';
            });
        }
    ) {}
    (builtins.attrNames entries);
in
  all // named

# Create derivations for a packages in a Spago workspace by providing the source
# directory containing its spago.yaml file and source code.
# See: https://github.com/purescript/spago/blob/master/spaghetto/core/src/Config.purs
{
  fromYAML,
  buildSpagoLock,
  lib,
}: rec {
  discoverLock = initPath: maxDepth: let
    # List of relative paths to try.
    paths = map (n: builtins.concatStringsSep "${initPath}/" (builtins.replicate n "..") + "/spago.lock") (builtins.range 1 maxDepth);

    # Function to try to read a file, returning either the contents of the file or null if the file does not exist.
    tryReadFile = path: let
      result = builtins.tryEval (buildSpagoLock.readSpagoLock path);
    in
      if result.success
      then result.value
      else null;
  in
    # Try to read each file in the list, returning the contents of the first one that exists.
    builtins.head (builtins.filter (x: x != null) (map tryReadFile paths));

  buildSpagoPackage = {
    src,
    spagoYaml ? src + "/spago.yaml",
    spagoLock,
    maxDepth ? 5,
  }: let
    config = fromYAML (builtins.readFile spagoYaml);
    lockPath =
      if spagoLock
      then spagoLock
      else discoverLock src maxDepth;
  in
    config;
}

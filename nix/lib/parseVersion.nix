{lib}: version: let
  splitVersion = builtins.split "-" version;
  majorMinorPatch = builtins.filter (x: x != []) (builtins.split "[.]" (builtins.head splitVersion));
  preRelease =
    if lib.length splitVersion == 2
    then builtins.elemAt splitVersion 1
    else "";
in
  if lib.length majorMinorPatch != 3
  then throw "Invalid SemVer: ${version}"
  else {
    major = builtins.head majorMinorPatch;
    minor = builtins.elemAt majorMinorPatch 1;
    patch = builtins.elemAt majorMinorPatch 2;
    preRelease = preRelease;
  }

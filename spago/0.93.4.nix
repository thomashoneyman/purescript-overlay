{
  callPackage,
  purs,
}:
callPackage ./mkSpago.nix {
  inherit purs;
  version = "0.93.4";
  rev = "116aaa37d6616c0a4721da268bdf9e6fb4219ae4"; # manual lookup of the commit, in this case
}

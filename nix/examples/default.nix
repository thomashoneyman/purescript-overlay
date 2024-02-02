# This file collects the packages used as minimal examples of the library in
# action, suitable for inclusion in the flake checks as a test suite.
{callPackage}: {
  simple = callPackage ./simple {};
  simple-ffi = callPackage ./simple-ffi {};
  simple-codegen = callPackage ./simple-codegen {};
  simple-tested = callPackage ./simple-tested {};
}

{
  system,
  fetchurl,
  lib,
  callPackage,
  ncurses,
}: let
  version = "0.15.10-0";

  # produced with nix-prefetch-url then nix hash to-sri --type sha256 <hash>
  urls = {
    "x86_64-linux" = {
      url = "https://github.com/purescript/purescript/releases/download/v${version}/linux64.tar.gz";
      hash = "sha256-Kc0PkV4kb7Kq9G7jvnkCfFQdClEf1mn14dF9m66dJVo=";
    };
    "x86_64-darwin" = {
      url = "https://github.com/purescript/purescript/releases/download/v${version}/macos.tar.gz";
      hash = "sha256-a5CTd3nbiAft/QarwOkAQkzHlxhxMUa1mb2FzjDElS0=";
    };
    "aarch64-darwin" = {
      url = "https://github.com/purescript/purescript/releases/download/v${version}/macos-arm64.tar.gz";
      hash = "sha256-nzvmUufMq2cvEYdbhBW2ZE5xncI9J/mup4Q7g6eeqks=";
    };
  };

  src =
    if builtins.hasAttr system urls
    then (fetchurl urls.${system})
    else if system == "aarch64-darwin"
    then let
      arch = "x86_64-darwin";
      msg = "Using the non-native ${arch} binary. While this binary may run under Rosetta 2 translation, no guarantees can be made about stability or performance.";
    in
      lib.warn msg (fetchurl urls.${arch})
    else throw "Architecture not supported: ${system}";
in
  callPackage ./mkPurs.nix {
    inherit version src ncurses;
  }

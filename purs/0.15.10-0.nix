{
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

in
  callPackage ../nix/mkPursDerivation.nix { inherit version urls ncurses; }

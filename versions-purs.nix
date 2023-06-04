{ callPackage, ncurses5, }: rec {
  purs = purs-0_15_9;
  purs-unstable = purs-0_15_10-0;

  purs-0_15_10-0 = callPackage ./nix/mkPursDerivation.nix rec {
    ncurses = ncurses5;
    version = "0.15.10-0";
    tarballs = {
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
  };

  purs-0_15_9 = callPackage ./nix/mkPursDerivation.nix rec {
    ncurses = ncurses5;
    version = "0.15.9";
    tarballs = {
      "x86_64-linux" = {
        url = "https://github.com/purescript/purescript/releases/download/v${version}/linux64.tar.gz";
        hash = "sha256-A8v0N75PGMT4fP9v3gUnm4ZmZDz7D+BM0As1TaeNS2U=";
      };
      "x86_64-darwin" = {
        url = "https://github.com/purescript/purescript/releases/download/v${version}/macos.tar.gz";
        hash = "sha256-LuGl4ChXbbs+6dz46++3kHjfgot8NT6OS5EeR3M6r/c=";
      };
      "aarch64-darwin" = {
        url = "https://github.com/purescript/purescript/releases/download/v${version}/macos-arm64.tar.gz";
        hash = "sha256-Sni8ez6TPPcqYvRhmD/HjAiTfBJB/mC9rR/w/K4RkZk=";
      };
    };
  };
}

rec {
  version = "0.15.8";
  tarballs = {
    "x86_64-linux" = {
      url = "https://github.com/purescript/purescript/releases/download/v${version}/linux64.tar.gz";
      hash = "sha256-Hoh/VpCNXsK5+ql1nMqVS3Qw+6PRMKjgVzEoSVTqV6Q=";
    };
    "x86_64-darwin" = {
      url = "https://github.com/purescript/purescript/releases/download/v${version}/macos.tar.gz";
      hash = "sha256-N+fc/8D8ERwEe290mSGm72Tr+hVXBcHqw4NSwT1aBis=";
    };
  };
}

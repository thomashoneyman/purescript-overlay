# Builds native (Haskell-compiled) PureScript tools from pre-built binaries.
# Used for: purs compiler, legacy spago (< 0.90.0)
{
  lib,
  stdenv,
  fetchurl,
  zlib,
  gmp,
  ncurses5,
  installShellFiles,
}:
{
  pname,
  meta,
  # Path to binary inside tarball (null = binary is at tarball root)
  binaryPath ? null,
  # Whether to generate shell completions
  generateCompletions ? true,
  # Function: version -> bool, whether the binary needs patchelf
  needsPatching ? _: true,
}:
version: source:
let
  shouldPatch = needsPatching version;

  dynamicLinker = stdenv.cc.bintools.dynamicLinker;

  buildInputs = lib.optionals shouldPatch [
    zlib
    gmp
    ncurses5
    stdenv.cc.cc.lib
  ];

  libPath = lib.makeLibraryPath buildInputs;

  patchelfCmd =
    if stdenv.isDarwin || !shouldPatch then
      ""
    else
      ''
        chmod u+w $BIN
        patchelf --interpreter ${dynamicLinker} --set-rpath ${libPath} $BIN
        chmod u-w $BIN
      '';

  # If binaryPath is null, the tarball contains just the binary at root level
  hasFlatTarball = binaryPath == null;
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchurl { inherit (source) url hash; };

  inherit buildInputs;
  nativeBuildInputs = lib.optionals generateCompletions [ installShellFiles ];

  dontStrip = true;

  # For flat tarballs, extract directly to a temp location
  unpackPhase = lib.optionalString hasFlatTarball ''
    runHook preUnpack
    mkdir -p unpacked
    tar xf $src -C unpacked
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    BIN=$out/bin/${pname}
  ''
  + (
    if hasFlatTarball then
      ''
        install -D -m555 -T unpacked/${pname} $BIN
      ''
    else
      ''
        install -D -m555 -T ${binaryPath} $BIN
      ''
  )
  + patchelfCmd
  + lib.optionalString generateCompletions ''
    installShellCompletion --cmd ${pname} \
      --bash <($BIN --bash-completion-script $BIN)
  ''
  + ''
    runHook postInstall
  '';

  meta = meta // {
    mainProgram = pname;
  };
}

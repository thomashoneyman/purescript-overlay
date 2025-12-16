# This derivation exposes all the tools described by the manifests in this
# directory.
{
  lib,
  system,
  callPackage,
  nodejs_20,
  nodejs,
  python3,
  darwin,
  nodePackages,
  installShellFiles,
  makeWrapper,
  stdenv,
}:
let
  mkNodeDerivation = callPackage ./build-support/mkNodeDerivation.nix { };
  mkNativeDerivation = callPackage ./build-support/mkNativeDerivation.nix { };

  # Spago-specific configuration for the Node.js-based versions
  spagoConfig = {
    # Spago < 0.93.45 requires Node.js 20 due to better-sqlite3 build issues
    selectNodejs = version: if lib.versionOlder version "0.93.45" then nodejs_20 else nodejs;

    # Native build inputs needed for better-sqlite3
    extraNativeBuildInputs = [
      nodePackages.node-gyp
      python3
      makeWrapper
      installShellFiles
    ]
    ++ lib.optionals stdenv.isDarwin [ darwin.cctools ];

    # Custom postInstall: wrapper for correct program name + shell completions
    postInstall =
      { nodejs, binPath, ... }:
      ''
        rm $out/bin/spago
        ln -s $out/lib/node_modules/spago/${binPath} $out/lib/node_modules/spago/bin/spago
        makeWrapper ${nodejs}/bin/node $out/bin/spago \
          --add-flags $out/lib/node_modules/spago/bin/spago

        installShellCompletion --cmd spago \
          --bash <($out/bin/spago --bash-completion-script $out/bin/spago) \
          --zsh <($out/bin/spago --zsh-completion-script $out/bin/spago) \
          --fish <($out/bin/spago --fish-completion-script $out/bin/spago)
      '';
  };

  # Tool configurations
  tools = {
    purs = {
      mkDrv = mkNativeDerivation {
        pname = "purs";
        binaryPath = "purs";
        # purs >= 0.15.16-7 is statically linked
        needsPatching = version: lib.versionOlder version "0.15.16-7";
        meta = {
          description = "Compiler for a strongly-typed language that compiles to JavaScript";
          homepage = "https://github.com/purescript/purescript";
          license = lib.licenses.bsd3;
        };
      };
      getEntry = entry: entry.${system} or null;
    };

    spago-legacy = {
      mkDrv = mkNativeDerivation {
        pname = "spago";
        meta = {
          description = "Package manager for PureScript";
          homepage = "https://github.com/purescript/spago";
          license = lib.licenses.bsd3;
        };
      };
      getEntry =
        entry:
        let
          systemEntry = entry.${system} or null;
        in
        # Legacy entries have system-specific URLs; new entries have tarball/depsHash
        if systemEntry != null && !(entry ? tarball) && !(entry ? url) then systemEntry else null;
    };

    spago = {
      mkDrv = mkNodeDerivation {
        pname = "spago";
        binPath = "bin/bundle.js";
        inherit (spagoConfig) selectNodejs extraNativeBuildInputs postInstall;
        meta = {
          description = "PureScript package manager and build tool";
          homepage = "https://github.com/purescript/spago";
          license = lib.licenses.bsd3;
        };
      };
      getEntry =
        entry:
        # New spago entries have tarball or url at top level (not system-specific)
        if entry ? tarball || entry ? url then entry else null;
    };

    purs-tidy = {
      mkDrv = mkNodeDerivation {
        pname = "purs-tidy";
        binPath = "bin/index.js";
        meta = {
          description = "PureScript formatter and tidy-upper";
          homepage = "https://github.com/natefaubion/purescript-tidy";
          license = lib.licenses.mit;
        };
      };
      getEntry = entry: entry;
    };

    purs-backend-es = {
      mkDrv = mkNodeDerivation {
        pname = "purs-backend-es";
        binPath = "index.js";
        meta = {
          description = "An optimizing backend toolkit for PureScript's CoreFn";
          homepage = "https://github.com/aristanetworks/purescript-backend-optimizer";
          license = lib.licenses.mit;
        };
      };
      getEntry = entry: entry;
    };

    purescript-language-server = {
      mkDrv = mkNodeDerivation {
        pname = "purescript-language-server";
        binPath = null;
        singleJsFile = true;
        meta = {
          description = "A Node-based language server protocol for PureScript";
          homepage = "https://github.com/nwolverson/purescript-language-server";
          license = lib.licenses.mit;
        };
      };
      getEntry = entry: entry;
    };
  };

  # Build a versioned package set from a JSON manifest
  mkManifest =
    {
      prefix,
      jsonFile,
      mkDrv,
      getEntry,
    }:
    let
      entries = builtins.fromJSON (builtins.readFile jsonFile);
    in
    lib.pipe (builtins.attrNames entries) [
      (map (
        version:
        let
          entry = getEntry entries.${version};
          name = "${prefix}-${builtins.replaceStrings [ "." ] [ "_" ] version}";
        in
        if entry == null then
          null
        else
          {
            inherit name;
            value = mkDrv version entry;
          }
      ))
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ];

  # Merge multiple manifests into one package set (for spago which has legacy + new)
  mkMergedManifest =
    {
      prefix,
      jsonFile,
      variants,
    }:
    let
      entries = builtins.fromJSON (builtins.readFile jsonFile);
    in
    lib.pipe (builtins.attrNames entries) [
      (map (
        version:
        let
          name = "${prefix}-${builtins.replaceStrings [ "." ] [ "_" ] version}";
          # Try each variant until one matches
          findDrv =
            vs:
            if vs == [ ] then
              null
            else
              let
                v = builtins.head vs;
                entry = v.getEntry entries.${version};
              in
              if entry != null then v.mkDrv version entry else findDrv (builtins.tail vs);
          drv = findDrv variants;
        in
        if drv == null then
          null
        else
          {
            inherit name;
            value = drv;
          }
      ))
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ];

  # Build all package sets
  purs-bin = mkManifest {
    prefix = "purs";
    jsonFile = ./purs.json;
    inherit (tools.purs) mkDrv getEntry;
  };

  spago-bin = mkMergedManifest {
    prefix = "spago";
    jsonFile = ./spago.json;
    variants = [
      tools.spago-legacy
      tools.spago
    ];
  };

  purs-tidy-bin = mkManifest {
    prefix = "purs-tidy";
    jsonFile = ./purs-tidy.json;
    inherit (tools.purs-tidy) mkDrv getEntry;
  };

  purs-backend-es-bin = mkManifest {
    prefix = "purs-backend-es";
    jsonFile = ./purs-backend-es.json;
    inherit (tools.purs-backend-es) mkDrv getEntry;
  };

  purescript-language-server-bin = mkManifest {
    prefix = "purescript-language-server";
    jsonFile = ./purescript-language-server.json;
    inherit (tools.purescript-language-server) mkDrv getEntry;
  };

  all = {
    inherit
      purs-bin
      spago-bin
      purs-tidy-bin
      purs-backend-es-bin
      purescript-language-server-bin
      ;
  };

  # Map named.json entries to actual packages.
  # Fails at evaluation time if a package isn't available for this system.
  named =
    let
      entries = builtins.fromJSON (builtins.readFile ./named.json);
    in
    lib.mapAttrs' (
      key: value:
      let
        name = builtins.replaceStrings [ "-stable" "-unstable" ] [ "" "" ] key;
        group = "${name}-bin";
        available = builtins.attrNames (all.${group} or { });
      in
      {
        # Strip -stable suffix but keep -unstable for unstable packages
        name = builtins.replaceStrings [ "-stable" ] [ "" ] key;
        value =
          all.${group}.${value} or (throw ''
            No binary available for ${key} (${value}) on system ${system}.
            Available versions: ${builtins.concatStringsSep ", " available}
          '');
      }
    ) entries;
in
all // named

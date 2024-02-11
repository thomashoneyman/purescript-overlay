{
  stdenv,
  lib,
  fetchurl,
  fetchgit,
  jq,
  rsync,
  # The following are present via the overlay
  fromYAML,
  pkgs,
}: rec {
  # Helper function for throwing an exception in case a constructed path does
  # not exist.
  pathExists = path:
    if builtins.pathExists path
    then path
    else throw "Path does not exist: ${path}";

  # Read the Spago lock file
  readSpagoLock = lockfile: fromYAML (builtins.readFile lockfile);

  # Read a package listed in the lockfile
  readLockedPackage = src: name: attr:
    if attr.type == "registry"
    then readRegistryPackage name attr
    else if attr.type == "git"
    then readGitPackage name attr
    else if attr.type == "local"
    then readLocalPackage src name attr
    else throw "Unknown package type ${attr.type}";

  # Option 1: Fetch and unpack the given package from the registry
  # "effect": {
  #   "type": "registry",
  #   "version": "4.0.0",
  #   "integrity": "sha256-eBtZu+HZcMa5HilvI6kaDyVX3ji8p0W9MGKy2K4T6+M="
  # },
  readRegistryPackage = name: attr:
    stdenv.mkDerivation {
      name = name;
      version = attr.version;
      dependencies = attr.dependencies;

      src = fetchurl {
        name = "${name}-${attr.version}.tar.gz";
        hash = attr.integrity;
        url = "https://packages.registry.purescript.org/${name}/${attr.version}.tar.gz";
      };

      installPhase = ''
        cp -R . "$out"
      '';
    };

  # Option 2: Fetch the given package from a Git url. Requires a commit hash.
  # "console": {
  #   "type": "git",
  #   "url": "https://github.com/purescript/purescript-console.git",
  #   "rev": "3b83d7b792d03872afeea5e62b4f686ab0f09842"
  # },
  readGitPackage = name: attr: let
    fetched = builtins.fetchGit {
      inherit (attr) url rev;
      # Look at commit hashes across the repository, not just the default branch,
      # in case they are pointing to a non-default-branch commit.
      allRefs = true;
    };
  in
    stdenv.mkDerivation {
      name = name;
      dependencies = attr.dependencies;
      src =
        if builtins.hasAttr "subdir" attr
        then "${fetched}/${attr.subdir}"
        else fetched;
      installPhase = ''
        cp -R . "$out"
      '';
    };

  # Option 3: Fetch the given package from a local path
  # "my-library": {
  #   "type": "local",
  #   "path": "my-library"
  # },
  readLocalPackage = src: name: attr:
    stdenv.mkDerivation {
      name = name;
      dependencies = attr.dependencies;
      src = pathExists "${src}/${attr.path}";
      installPhase = ''
        cp -R . "$out"
      '';
    };

  # Read all the locked packages
  lockedPackages = src: lock:
    lib.mapAttrs (readLockedPackage src) lock.packages;

  # Read a workspace package. These are listed at the top of the
  # lockfile, not in the main packages list.
  readWorkspacePackage = src: extraSrcs: name: attr:
    stdenv.mkDerivation {
      name = name;
      # The workspace packages list version ranges with their dependencies, so
      # we want to take only the keys.
      dependencies = let
        toNames = dep:
          if builtins.typeOf dep == "set"
          then lib.attrNames dep
          else [dep];
      in
        lib.concatMap toNames attr.dependencies;
      src =
        if attr.path == "./"
        then src
        else pathExists "${src}/${attr.path}";
      installPhase = ''
        cp -R . $out
        ${
          if builtins.hasAttr name extraSrcs
          then ''
            echo "Copying extra sources from ${extraSrcs.${name}}..."
            cp -r ${extraSrcs.${name}} $out/src
          ''
          else ''
            echo "No extra sources to copy for ${name}."
          ''
        }
      '';
    };

  # Read the workspace packages
  workspacePackages = src: extraSrcs: lock:
    lib.mapAttrs (readWorkspacePackage src extraSrcs) lock.workspace.packages;

  # Merges together the dependencies into a local output directory such that
  # they can be used for incremental compilation.
  syncOutput = deps: let
    options = {
      "recursive" = true;
      "checksum" = true;
      "chmod" = "u+w";
      "no-owner" = true;
      "no-group" = true;
      "times" = true;
      "links" = true;
      "mkpath" = true;
    };
  in "${rsync}/bin/rsync ${lib.cli.toGNUCommandLineShell {} options} ${lib.concatMapStringsSep " " (dep: "${dep}/output") deps} .";

  # Merge the cache-db.json files of all the dependencies so the compiler knows
  # not to rebuild them.
  mergeCacheDb = deps: ''
    caches=()
    for dir in ${lib.concatStringsSep " " deps}
    do
      caches+="$dir/output/cache-db.json "
    done
    # Merge cache-db.json files and write them to the output directory
    jq -s 'reduce .[] as $item ({}; reduce ($item|keys_unsorted[]) as $key (.; .[$key] += $item[$key]))' $caches > output/cache-db.json
  '';

  fixDependencies = {
    purs,
    corefn,
  }: deps:
    lib.fix (self:
      lib.mapAttrs (name: drv: let
        get-dep = dep: self.${dep};

        directs = builtins.listToAttrs (map (name: {
            name = name;
            value = get-dep name;
          })
          drv.dependencies);

        transitive = builtins.foldl' (a: pkg: a // self.${pkg}.dependencies) {} drv.dependencies;

        dependencies = transitive // directs;

        defaultVersion = "0.0.0";

        version =
          drv.version
          or (
            if builtins.pathExists "${drv.out}/spago.yaml"
            then lib.attrByPath ["package" "publish" "version"] defaultVersion (fromYAML (builtins.readFile "${drv.out}/spago.yaml"))
            else defaultVersion
          );

        # FIXME hack to provide spago and spago-bin with spagoVersion
        dependenciesList =
          lib.attrValues dependencies
          ++ [
            {
              name = "spago-bin";
              version = version;
            }
          ];

        renderPackageType = p: ''"${p.name}" :: String'';
        packagesType = "{ ${lib.concatMapStringsSep ", " renderPackageType dependenciesList} }";
        renderPackage = p: ''"${p.name}": "${p.version or defaultVersion}"'';
        packages = ''{ ${lib.concatMapStringsSep "\n  , " renderPackage dependenciesList} }'';
      in
        stdenv.mkDerivation rec {
          name = drv.name;

          src = drv.out;

          inherit version;

          nativeBuildInputs = [purs jq];

          phases = ["buildPhase" "installPhase" "checkPhase"];

          passthru = {
            inherit dependencies;
          };

          # TODO: The 'files' key can indicate additional deps.
          globs =
            ["${src}/src/**/*.purs"]
            ++ map (dep: ''"${dep.src}/src/**/*.purs"'') (builtins.attrValues dependencies);

          # This is bad...but without Spago, how else can we get this?
          buildInfo = pkgs.writeText "BuildInfo.purs" ''
            -- @inline export packages always
            -- @inline export pursVersion always
            -- @inline export spagoVersion always
            module Spago.Generated.BuildInfo where

            packages :: ${packagesType}
            packages = ${packages}

            pursVersion :: String
            pursVersion = "${purs.version}"

            spagoVersion :: String
            spagoVersion = "${version}"
          '';

          preBuild = ''
            mkdir output
            echo "Fetching dependencies..."
            ${syncOutput (builtins.attrValues directs)}
            echo "Merge cache-db.json files..."
            ${mergeCacheDb (builtins.attrValues directs)}
          '';

          buildPhase = ''
            runHook preBuild

            cleanup() {
              exit_code=$?
              if [ $exit_code -ne 0 ]; then
                echo "purs compile failed with exit code $exit_code."
                cat purs-log.txt
              fi
              exit $exit_code
            }

            trap cleanup EXIT

            # TODO: Ideally we would only compile to corefn if we know it's
            # necessary (for example, a 'backend' command was supplied).
            set -f
            purs compile ${lib.concatStringsSep " " globs} ${buildInfo} --codegen js${
              if corefn
              then ",corefn"
              else ""
            } 2>&1 | tee purs-log.txt
            set +f
          '';

          installPhase = ''
            mkdir $out
            mv output $out/output
          '';

          # We can add tests here, but they're off by default.
          doCheck = false;
          checkPhase = ''
            FILE_COUNT=$(find ${src}/src -name '*.purs' | wc -l)
            COMPILE_LINE=$(grep -m 1 "^\[[0-9]* of [0-9]*\] Compiling" purs-log.txt)
            MODULE_COUNT=$(echo $COMPILE_LINE | sed -n 's/^\[\([0-9]*\) of \([0-9]*\)\] Compiling.*$/\2/p')
            if [[ $FILE_COUNT -ne $MODULE_COUNT ]]; then
              echo ".purs file count ($FILE_COUNT) does not match the module count ($MODULE_COUNT)."
              echo "This indicates incremental compilation is wrong."
              cat purs-log.txt
              exit 1
            fi
          '';
        })
      deps);

  buildSpagoLock = {
    src,
    corefn ? false,
    purs ? pkgs.purs,
    lockfile ? src + "/spago.lock",
    # A record from a name of a spago package in this workspace to a derivation with additional
    # source files. This is useful for injecting generated code (e.g. from `graphql-client`).
    #
    # Non-existent packages are silently ignored.
    extraSrcs ? {},
  }: let
    lock = readSpagoLock lockfile;
    workspaceDirs = builtins.attrValues (lib.mapAttrs (_: attr: attr.path) lock.workspace.packages);
    # We only want to include the lockfile and code from any listed workspaces
    filteredSrc = lib.cleanSourceWith {
      filter = name: type: name != "spago.lock" && !(builtins.elem name workspaceDirs);
      src = lib.cleanSource src;
    };
  in
    fixDependencies {inherit purs corefn;}
    (lockedPackages filteredSrc lock // workspacePackages filteredSrc extraSrcs lock);
}

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
    (
      if attr.type == "registry"
      then readRegistryPackage name attr
      else if attr.type == "git"
      then readGitPackage name attr
      else if attr.type == "local"
      then readLocalPackage src name attr
      else throw "Unknown package type ${attr.type}"
    )
    // {fromWorkspace = false;};

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
      depNames = attr.dependencies;

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
      depNames = attr.dependencies;
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
      depNames = attr.dependencies;
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
  readWorkspacePackage = src: extraSrcs: name: attr: let
    toNames = dep:
      if builtins.typeOf dep == "set"
      then lib.attrNames dep
      else [dep];
  in
    stdenv.mkDerivation {
      name = name;

      # The workspace packages list version ranges with their dependencies, so
      # we want to take only the keys.
      depNames = lib.concatMap toNames attr.dependencies;

      testDepNames =
        if attr ? test_dependencies
        then lib.concatMap toNames attr.test_dependencies
        else {};

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

      fromWorkspace = true;
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
    if [ -f output/cache-db.json ]; then
      caches+="output/cache-db.json "
    fi
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
    workspaceSrc,
    tests,
    npmDependencies,
  }: spagoPkgs:
    lib.fix (
      self:
        lib.mapAttrs (name: spagoPkg: let
          depNameToAttr = name: {
            name = name;
            value = self.${name};
          };

          makeTransitiveDeps = builtins.foldl' (a: depName: a // self.${depName}.deps) {};

          directs = builtins.listToAttrs (map depNameToAttr spagoPkg.depNames);

          transitive = makeTransitiveDeps spagoPkg.depNames;

          deps = transitive // directs;

          defaultVersion = "0.0.0";

          version =
            spagoPkg.version
            or (
              if builtins.pathExists "${spagoPkg.out}/spago.yaml"
              then lib.attrByPath ["package" "publish" "version"] defaultVersion (fromYAML (builtins.readFile "${spagoPkg.out}/spago.yaml"))
              else defaultVersion
            );

          # FIXME hack to provide spago and spago-bin with spagoVersion
          depsList =
            lib.attrValues deps
            ++ [
              {
                name = "spago-bin";
                version = version;
              }
            ];

          renderPackageType = p: ''"${p.name}" :: String'';
          packagesType = "{ ${lib.concatMapStringsSep ", " renderPackageType depsList} }";
          renderPackage = p: ''"${p.name}": "${p.version or defaultVersion}"'';
          packages = ''{ ${lib.concatMapStringsSep "\n  , " renderPackage depsList} }'';
        in
          stdenv.mkDerivation rec {
            name = spagoPkg.name;

            src = spagoPkg.out;

            inherit version;

            nativeBuildInputs =
              [purs jq]
              # Nodejs is only needed if there is a tested package.
              ++ (
                if runSpagoTests
                then [pkgs.nodejs]
                else []
              );

            phases = ["buildPhase" "installPhase" "checkPhase"];

            passthru = {
              inherit deps;
            };

            # TODO: The 'files' key can indicate additional deps.
            globs =
              ["${src}/src/**/*.purs"]
              ++ map (dep: ''"${dep.src}/src/**/*.purs"'') (builtins.attrValues deps);

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

              # Cancel trap.
              trap - EXIT
            '';

            installPhase = ''
              mkdir $out
              cp -r output $out
            '';

            runSpagoTests = spagoPkg.fromWorkspace && builtins.hasAttr spagoPkg.name tests;

            doCheck = true;
            checkPhase = ''
              # The +1 is for the BuildInfo.purs file.
              FILE_COUNT=$(($(find ${src}/src -name '*.purs' | wc -l) + 1))
              COMPILE_LINE=$(grep -m 1 "^\[[0-9]* of [0-9]*\] Compiling" purs-log.txt)
              MODULE_COUNT=$(echo $COMPILE_LINE | sed -n 's/^\[\([0-9]*\) of \([0-9]*\)\] Compiling.*$/\2/p')
              if [[ $FILE_COUNT -ne $MODULE_COUNT ]]; then
                echo ".purs file count ($FILE_COUNT) does not match the module count ($MODULE_COUNT)."
                echo "This indicates incremental compilation is wrong."
                cat purs-log.txt
                exit 1
              fi
              ${
                let
                  testDepNames = spagoPkg.testDepNames or [];
                  directTestDeps = builtins.listToAttrs (map depNameToAttr testDepNames);
                  transitiveTestDeps = makeTransitiveDeps testDepNames;
                  testDeps = transitiveTestDeps // directTestDeps;
                  testGlobs =
                    globs
                    ++ ["${src}/test/**/*.purs"]
                    ++ map (dep: ''"${dep.src}/src/**/*.purs"'') (builtins.attrValues testDeps);

                  spagoTestCommands = assert lib.assertMsg (npmDependencies != null) "npmDependencies must be set to run tests"; ''
                    echo "Compiling tests..."
                    set -f
                    purs compile ${lib.concatStringsSep " " testGlobs} --codegen js 2>&1
                    set +f
                    ${
                      # Only symlink if the package-lock.json exists.
                      if builtins.pathExists "${workspaceSrc}/package-lock.json"
                      then ''
                        echo "Symlinking node modules..."
                        ln -s ${npmDependencies} node_modules
                        ln -s ${workspaceSrc}/package-lock.json .
                      ''
                      else "echo 'No package-lock.json found in ${src}. Skipping symlinking.'"
                    }
                    # Generate test_entrypoint.mjs.
                    echo "import { main } from './output/${tests."${spagoPkg.name}"}/index.js'; main();" > test_entrypoint.mjs
                    echo "Running tests..."
                    node test_entrypoint.mjs
                  '';
                in
                  lib.optionalString runSpagoTests spagoTestCommands
              }
            '';
          })
        spagoPkgs
    );

  buildSpagoLock = {
    src,
    corefn ? false,
    purs ? pkgs.purs,
    lockfile ? src + "/spago.lock",
    # The node_modules directory. Only needed for tests.
    npmDependencies ? null,
    # A record from a name of a spago package in this workspace to a derivation with additional
    # source files. This is useful for injecting generated code (e.g. from `graphql-client`).
    #
    # Non-existent packages are silently ignored.
    extraSrcs ? {},
    # A record from a name of a spago package in this workspace to the name of its test module.
    #
    # Non-existent packages are silently ignored.
    #
    # TODO: Don't require the user to pass the name of the main module of the test. It is in
    # stack.yaml, but we don't have a way to parse that yet.
    tests ? {},
  }:
    assert lib.assertMsg (builtins.isAttrs extraSrcs)
    "argument `extraSrcs` to `buildSpagoLock` must be a set";
    assert lib.assertMsg (builtins.isAttrs tests)
    "argument `tests` to `buildSpagoLock` must be a set"; let
      lock = readSpagoLock lockfile;
      workspaceDirs =
        builtins.attrValues (lib.mapAttrs (_: attr: attr.path) lock.workspace.packages);
      # We only want to include the lockfile and code from any listed workspaces
      workspaceSrc = lib.cleanSource src;
      filteredSrc = lib.cleanSourceWith {
        filter = name: type: name != "spago.lock" && !(builtins.elem name workspaceDirs);
        src = workspaceSrc;
      };
    in
      fixDependencies {inherit purs corefn tests npmDependencies workspaceSrc;}
      (lockedPackages filteredSrc lock // workspacePackages filteredSrc extraSrcs lock);
}

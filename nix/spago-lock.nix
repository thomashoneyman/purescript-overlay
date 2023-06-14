{
  stdenv,
  lib,
  fetchurl,
  fetchgit,
  # The following are present via the overlay
  fromYAML,
}: rec {
  # Helper function for throwing an exception in case a constructed path does
  # not exist.
  pathExists = path:
    if builtins.pathExists path
    then path
    else throw "Path does not exist: ${path}";

  # Read the Spago config file
  readSpagoConfig = config: fromYAML (builtins.readFile config);

  # Read the Spago lock file
  readSpagoLock = lockfile: fromYAML (builtins.readFile lockfile);

  # Read a package listed in the lockfile
  readLockedPackage = name: attr:
    if attr.type == "registry"
    then readRegistryPackage name attr
    else if attr.type == "git"
    then readGitPackage name attr
    else if attr.type == "local"
    then readLocalPackage name attr
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
  lockedPackages = lock: lib.mapAttrs readLockedPackage lock.packages;

  workspaces = {
    lockfile,
    src,
  }: let
    lock = readSpagoLock lockfile;

    # Read the workspace packages
    workspacePackages = lib.mapAttrs readWorkspacePackage lock.workspace.packages;

    # Union all packages so they can be used as a little package database
    allPackages = lockedPackages lock // workspacePackages;

    closureEntry = name: {
      key = name;
      package = allPackages.${name} or (builtins.throw ''Missing package "${name}"'');
    };

    workspaceClosure = workspace:
      builtins.genericClosure {
        startSet = map closureEntry workspace.dependencies;
        operator = entry: map closureEntry entry.package.dependencies;
      };

    # Read a workspace package. These are listed at the top of the
    # lockfile, not in the main packages list.
    readWorkspacePackage = name: attr:
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
        '';
      };

    buildWorkspace = name: workspace: {
      dependencies = rec {
        # Get all dependencies of the workspace
        sources = map (pkg: pkg.package) (workspaceClosure workspace);
        # Turn them into glob patterns acceptable for the compiler
        globs = builtins.concatStringsSep " " (map (path: "'${path}/src/**/*.purs'") sources);
      };
    };
  in
    lib.mapAttrs buildWorkspace workspacePackages;
}

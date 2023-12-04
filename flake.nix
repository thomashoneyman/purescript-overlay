{
  description = "Nix derivations for PureScript core language tools.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";

    slimlock.url = "github:thomashoneyman/slimlock";
    slimlock.inputs.nixpkgs.follows = "nixpkgs";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    slimlock,
    flake-compat,
  }: let
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = forAllSystems (system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default slimlock.overlays.default];
      });
  in {
    overlays.default = import ./overlay.nix;

    # A warning-free top-level flake output suitable for running unit tests via
    # e.g. `nix eval .#lib`.
    lib = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      tests = pkgs.callPackage ./nix/tests {};
    in
      tests);

    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      purs = pkgs.purs;
      purs-unstable = pkgs.purs-unstable;
      purs-bin = pkgs.purs-bin;

      spago = pkgs.spago;
      spago-unstable = pkgs.spago-unstable;
      spago-bin = pkgs.spago-bin;

      purs-tidy = pkgs.purs-tidy;
      purs-tidy-unstable = pkgs.purs-tidy-unstable;
      purs-tidy-bin = pkgs.purs-tidy-bin;

      purs-backend-es = pkgs.purs-backend-es;
      purs-backend-es-unstable = pkgs.purs-backend-es-unstable;
      purs-backend-es-bin = pkgs.purs-backend-es-bin;

      purescript-language-server = pkgs.purescript-language-server;
      purescript-language-server-unstable = pkgs.purescript-language-server-unstable;
      purescript-language-server-bin = pkgs.purescript-language-server-bin;
    in
      {
        inherit purs purs-unstable spago spago-unstable purs-tidy purs-tidy-unstable purs-backend-es purs-backend-es-unstable purescript-language-server purescript-language-server-unstable;
      }
      // purs-bin
      // spago-bin
      // purs-tidy-bin
      // purs-backend-es-bin
      // purescript-language-server-bin);

    apps = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      mkApp = bin: {
        type = "app";
        program = "${bin}/bin/${bin.pname or bin.name}";
      };
      apps = pkgs.lib.mapAttrs (_: mkApp) self.packages.${system};
      scripts = {generate = mkApp (pkgs.callPackage ./generate {});};
    in
      apps // scripts);

    checks = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      package-checks =
        pkgs.lib.mapAttrs (key: bin: let
          name = bin.pname or bin.name;
          version = bin.version or "0.0.0";
        in
          pkgs.runCommand "test-${name}-${version}" {} ''
            touch $out
            set -e
            set -x

            # Some package versions are not supported on some systems, ie. the
            # "stable" version of Spago is not supported on aarch64.
            if [ ${builtins.toString (builtins.hasAttr "unsupported" bin)} ]; then
              echo "Skipping ${bin.name} because it is not supported on ${system}"
              exit 0
            fi

            # Different packages at different versions use different 'version'
            # flags to print their version
            if [ ${builtins.toString (name == "spago" && pkgs.lib.versionOlder version "0.90.0")} ]; then
              VERSION="$(${bin}/bin/${name} version --global-cache skip)"
            else
              # spago-next writes --version to stderr, oddly enough, so we need to
              # capture both in the VERSION var.
              VERSION="$(${bin}/bin/${name} --version 2>&1)"
            fi

            # purs-tidy includes a 'v' prefix in its output beginning with version 0.9.0
            if [ ${builtins.toString (name == "purs-tidy" && !(pkgs.lib.versionOlder version "0.9.0"))} ]; then
              EXPECTED_VERSION="v${version}"
            # purs-backend-es always includes it
            elif [ ${builtins.toString (name == "purs-backend-es")} ]; then
              EXPECTED_VERSION="v${version}"
            else
              EXPECTED_VERSION="${version}"
            fi

            echo "$VERSION should match expected output $EXPECTED_VERSION"
            test "$VERSION" = "$EXPECTED_VERSION"
          '')
        # TODO: Remove once the purescript build of spago is stable
        (pkgs.lib.filterAttrs (k: v: !(k == "spago" && system == "aarch64-darwin")) self.packages.${system});

      example-checks = pkgs.callPackages ./nix/examples {};

      script-checks = {
        generate = let
          bin = pkgs.callPackage ./generate {};
          manifests = ./manifests;
        in
          pkgs.runCommand "test-generate" {} ''
            mkdir -p $out/bin
            set -e
            set -x
            cp ${bin}/bin/${bin.name} $out/bin/test-generate
            ${bin}/bin/${bin.name} verify ${manifests}
          '';
      };
    in
      package-checks // example-checks // script-checks);

    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in {
      default = pkgs.mkShell {
        name = "purescript-overlay";
        buildInputs = [
          self.packages.${system}.spago-unstable
          self.packages.${system}.purs-unstable
          self.packages.${system}.purs-tidy-unstable
          self.packages.${system}.purs-backend-es-unstable
          self.packages.${system}.purescript-language-server-unstable

          pkgs.nodejs_20
          pkgs.prefetch-npm-deps
        ];
      };
    });
  };
}

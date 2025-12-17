{
  description = "Nix derivations for PureScript core language tools.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      eachDefaultSystem =
        perSystem:
        lib.pipe
          [
            "x86_64-linux"
            "x86_64-darwin"
            "aarch64-linux"
            "aarch64-darwin"
          ]
          [
            (map (sys: builtins.mapAttrs (_: value: { ${sys} = value; }) (perSystem sys)))
            (builtins.foldl' lib.recursiveUpdate { })
          ];
    in
    eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
          ];
        };
      in
      {
        # Filter out null values (packages unavailable on this system)
        packages = lib.filterAttrs (_: v: v != null) (
          {
            inherit (pkgs)
              purs
              purs-unstable
              spago
              spago-unstable
              purs-tidy
              purs-tidy-unstable
              purs-backend-es
              purs-backend-es-unstable
              purescript-language-server
              purescript-language-server-unstable
              ;
          }
          // pkgs.purs-bin
          // pkgs.spago-bin
          // pkgs.purs-tidy-bin
          // pkgs.purs-backend-es-bin
          // pkgs.purescript-language-server-bin
        );

        apps =
          let
            mkApp = bin: {
              type = "app";
              program = "${bin}/bin/${bin.pname or bin.name}";
              meta = bin.meta or { };
            };
          in
          lib.mapAttrs (_: mkApp) self.packages.${system};

        checks =
          let
            package-checks = lib.mapAttrs (
              _: bin:
              let
                name = bin.pname or bin.name;
                version = bin.version or "0.0.0";
              in
              pkgs.runCommand "test-${name}-${version}" { } ''
                touch $out
                set -e
                set -x

                # Different packages at different versions use different 'version'
                # flags to print their version
                if [ ${builtins.toString (name == "spago" && lib.versionOlder version "0.90.0")} ]; then
                  VERSION="$(${lib.getExe bin} version --global-cache skip)"
                # spago@0.93.21 incorrectly reports its version
                elif [ ${builtins.toString (name == "spago" && version == "0.93.21")} ]; then
                  VERSION="0.93.21"
                else
                  # spago-next writes --version to stderr, oddly enough, so we need to
                  # capture both in the VERSION var.
                  VERSION="$(${lib.getExe bin} --version 2>&1)"
                fi

                # purs-tidy includes a 'v' prefix in its output beginning with version 0.9.0
                if [ ${builtins.toString (name == "purs-tidy" && !(lib.versionOlder version "0.9.0"))} ]; then
                  EXPECTED_VERSION="v${version}"
                # purs-backend-es always includes it
                elif [ ${builtins.toString (name == "purs-backend-es")} ]; then
                  EXPECTED_VERSION="v${version}"
                else
                  EXPECTED_VERSION="${version}"
                fi

                echo "$VERSION should match expected output $EXPECTED_VERSION"
                test "$VERSION" = "$EXPECTED_VERSION"
              ''
            ) (lib.filterAttrs (k: _: !(k == "spago" && system == "aarch64-darwin")) self.packages.${system});
            # TODO: Remove filter once the purescript build of spago is stable on aarch64-darwin

            format-checks = {
              nix-format =
                pkgs.runCommand "nix-format"
                  {
                    src = lib.fileset.toSource {
                      root = ./.;
                      fileset = lib.fileset.fileFilter (f: f.hasExt "nix") ./.;
                    };
                    nativeBuildInputs = [ pkgs.nixfmt ];
                  }
                  ''
                    nixfmt --check $(find $src -type f) && touch $out
                  '';
            };
          in
          package-checks // format-checks;

        devShells = {
          default = pkgs.mkShell {
            name = "purescript-overlay";
            buildInputs = [
              self.packages.${system}.spago-unstable
              self.packages.${system}.purs-unstable
              self.packages.${system}.purs-tidy-unstable
              self.packages.${system}.purs-backend-es-unstable
              self.packages.${system}.purescript-language-server-unstable

              pkgs.nodejs
              pkgs.prefetch-npm-deps
              pkgs.nixfmt
            ];
          };
        };

        formatter = pkgs.nixfmt;
      }
    )
    // {
      overlays.default = import ./overlay.nix;
    };
}

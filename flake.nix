{
  description = "Nix derivations for PureScript core language tools.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    overlay = import ./overlay.nix;
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = forAllSystems (system:
      import nixpkgs {
        inherit system;
        overlays = [overlay];
      });
  in {
    overlays.default = overlay;

    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      purs = pkgs.purs;
      purs-unstable = pkgs.purs-unstable;
      purs-bin = pkgs.purs-bin;
      spago = pkgs.spago;
      spago-bin = pkgs.spago-bin;
    in
      {inherit purs purs-unstable spago;} // purs-bin // spago-bin);

    apps = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      mkApp = bin: {
        type = "app";
        program = "${bin}/bin/${bin.pname or bin.name}";
      };

      packages = pkgs.lib.mapAttrs (_: mkApp) self.packages.${system};

      scripts = {
        generate = mkApp (pkgs.callPackage ./generate {});
      };
    in
      packages // scripts);

    checks = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      package-checks = pkgs.lib.mapAttrs (name: bin:
        pkgs.runCommand "test-${name}" {} ''
          touch $out
          set -e
          # Spago writes --version to stderr, oddly enough, so we need to
          # capture both in the VERSION var.
          VERSION="$(${bin}/bin/${bin.pname} --version 2>&1)"
          EXPECTED_VERSION="${bin.version}"
          echo "$VERSION should match expected output $EXPECTED_VERSION"
          test "$VERSION" = "$EXPECTED_VERSION"
        '')
      self.packages.${system};

      test-checks = {
        test-generate = let
          bin = pkgs.callPackage ./generate {};
          manifests = ./manifests;
        in
          pkgs.runCommand "test-generate" {} ''
            mkdir -p $out/bin
            set -e
            cp ${bin}/bin/${bin.name} $out/bin/test-generate
            ${bin}/bin/${bin.name} verify ${manifests}
          '';
      };
    in
      test-checks // package-checks);

    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in {
      default = pkgs.mkShell {
        name = "purescript-nix";
        buildInputs = [
          self.packages.${system}.spago
          self.packages.${system}.purs
        ];
      };
    });
  };
}

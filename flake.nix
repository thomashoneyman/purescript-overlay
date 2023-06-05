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
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
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
      scripts = {
        generate = {
          type = "app";
          program = let
            script = pkgs.callPackage ./generate {};
          in "${script}/bin/${script.name}";
        };
      };
      tools =
        pkgs.lib.mapAttrs (_: bin: {
          type = "app";
          program = "${bin}/bin/${bin.pname}";
        })
        self.packages.${system};
    in
      tools // scripts);

    devShells = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in {
      default = pkgs.mkShell {
        name = "purescript-nix";
        buildInputs = [
          self.packages.${system}.spago-0_93_4
          self.packages.${system}.purs-0_15_8
        ];
      };
    });

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
        in
          pkgs.runCommand "test-generate" {} ''
            mkdir -p $out/bin
            set -e
            cp ${bin}/bin/app $out/bin/test-generate
            ${bin}/bin/app --verify
          '';
      };
    in
      test-checks // package-checks);
  };
}

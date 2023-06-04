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
    extractPrefix = lib: str:
      if lib.hasPrefix "purs" str
      then "purs"
      else if lib.hasPrefix "spago" str
      then "spago"
      else (throw "Expected 'purs' or 'spago' prefix but none was found: ${str}");
  in {
    overlays.default = overlay;

    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      purs = pkgs.purs;
      spago = pkgs.spago;
    in
      purs // spago);

    apps = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      prefix = extractPrefix pkgs.lib;
      apps =
        pkgs.lib.mapAttrs (name: bin: {
          type = "app";
          program = "${bin}/bin/${prefix name}";
        })
        self.packages.${system};
    in
      apps);

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

    checks = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};

      prefix = extractPrefix pkgs.lib;
      package-checks = pkgs.lib.mapAttrs (name: bin:
        pkgs.runCommand "test-${name}" {buildInputs = [bin];} ''
          touch $out
          set -e
          # Spago writes --version to stderr, oddly enough, so we need to
          # capture both in the VERSION var.
          VERSION="$(${bin}/bin/${prefix name} --version 2>&1)"
          EXPECTED_VERSION="${bin.version}"
          echo "$VERSION should match expected output $EXPECTED_VERSION"
          test "$VERSION" = "$EXPECTED_VERSION"
        '')
      self.packages.${system};

      example-project = pkgs.callPackage ./example {};
      example-checks = {
        test-example = pkgs.runCommand "test-example" {buildInputs = [example-project];} ''
          mkdir -p $out/bin
          set -e
          cp ${example-project}/bin/my-app $out/bin/test-example
          ${example-project}/bin/my-app
        '';
      };
    in
      example-checks // package-checks);
  };
}

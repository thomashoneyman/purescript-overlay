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
      spago = pkgs.spago;
    in
      purs // spago);

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
          program = "${bin}/bin/${bin.name}";
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

      package-checks = pkgs.lib.mapAttrs (name: bin: let
        test-script = ''
          touch $out
          set -e
          # Spago writes --version to stderr, oddly enough, so we need to
          # capture both in the VERSION var.
          VERSION="$(${bin}/bin/${bin.pname} --version 2>&1)"
          EXPECTED_VERSION="${bin.version}"
          echo "$VERSION should match expected output $EXPECTED_VERSION"
          test "$VERSION" = "$EXPECTED_VERSION"
        '';
      in
        # Garnix aarch64-darwin builders do not have Rosetta 2 installed, which
        # allows for running x86_64-darwin code on an aarch64-darwin system.
        if bin.needsRosetta or false
        then
          pkgs.runCommand "test-${name}" {} ''
            if sysctl sysctl.proc_translated > /dev/null 2>&1; then
              echo "Needs translation and Rosetta 2 is installed, running tests..."
              ${test-script}
            else
              echo "Needs translation and Rosetta 2 is not installed, binary will not work, skipping..."
              touch $out
            fi
          ''
        else
          pkgs.runCommand "test-${name}" {} ''
            echo "Does not need translation, running tests..."
            ${test-script}
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

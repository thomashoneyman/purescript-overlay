{
  description = "Nix derivations for PureScript core language tools.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = forAllSystems (system: import nixpkgs {inherit system;});
  in {
    packages = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      purs = pkgs.callPackages ./purs/versions.nix {};
    in
      purs);

    apps = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      apps =
        pkgs.lib.mapAttrs (name: purs-bin: {
          type = "app";
          program = "${purs-bin}/bin/purs";
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
          self.packages.${system}.purs
        ];
      };
    });

    checks = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
      purs = pkgs.lib.mapAttrs (name: purs-bin:
        pkgs.runCommand "purs" {buildInputs = [purs-bin];} ''
          touch $out
          set -e
          PURS_VERSION=$(purs --version)
          EXPECTED_VERSION="${purs-bin.version}"
          echo "$PURS_VERSION should match expected output $EXPECTED_VERSION"
          test "$PURS_VERSION" = "$EXPECTED_VERSION"
        '')
      self.packages.${system};
    in
      purs);
  };
}

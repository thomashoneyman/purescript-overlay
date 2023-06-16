# Purix

[![daily-update](https://github.com/thomashoneyman/purescript-nix/actions/workflows/daily-update.yaml/badge.svg)](https://github.com/thomashoneyman/purescript-nix/actions/workflows/daily-update.yaml)
[![darwin-support](https://github.com/thomashoneyman/purescript-nix/actions/workflows/darwin-support.yaml/badge.svg)](https://github.com/thomashoneyman/purescript-nix/actions/workflows/darwin-support.yaml)
[![nix-unit-tests](https://github.com/thomashoneyman/purescript-nix/actions/workflows/nix-unit-tests.yaml/badge.svg)](https://github.com/thomashoneyman/purescript-nix/actions/workflows/nix-unit-tests.yaml)

Purix is two things:

1. An overlay and flake exposing the core PureScript toolchain — `purs`, `spago`, `purs-tidy`, and `purs-backend-es` — at both stable and pre-release versions, with daily auto-updates for new releases.
2. A library suitable for building PureScript projects with Nix, called `purix`.

Purix is tested on the following architectures:

- x86_64-linux
- x86_64-darwin (Intel Mac)
- aarch64-darwin (M1 Mac)
- aarch64-linux

The included overlay inserts the latest stable and unstable executables into your packages (ie. `purs`, `purs-unstable`, and so on). You can see all specific versions in the [named.json](./manifests/named.json) file. It also provides many versions of each executable under a `-bin` namespace (ie. `purs-bin`, `spago-bin`, and so on) so you can access specific versions of a tool. For example, you can use `purs-bin.purs-0_15_8` to get the 0.15.8 PureScript compiler. These are tracked in the [manifests](./manifests/) directory.

The included library provides helper functions for building PureScript packages, namely:

- `buildSpagoLock`: Build `output` directories for any package or workspace listed in a spago.lock file
- `buildPackageLock`: Install all dependencies in a package-lock.json file into a node_modules directory.

## Usage

In a Nix flake, use the provided overlay when importing nixpkgs to get access to tools like `purs` and `spago` and functions like `buildSpagoLock`. For example, the below flake creates a development shell with recent versions of the PureScript compiler and Spago package manager:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-nix.url = "github:thomashoneyman/purescript-nix";
    purescript-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, purescript-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-nix.overlays.default ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.purs-unstable
            pkgs.spago-bin.spago-0_93_5
          ];
        };
      }
    );
}
```

You can also run individual packages from the flake, e.g.

```console
nix run github:thomashoneyman/purescript-nix#purs-unstable
```

## Examples

The [`generate`](./generate/) directory contains a PureScript script implemented using this library. It has two Spago workspaces and foreign Node dependencies which are packaged into an runnable script via the utilities in this library. Specifically, see the included [`default.nix`](./generate/default.nix).

```console
nix run .#generate
```

## Tests

You can run the repository tests using a combination of `nix eval .#lib` (to run the unit tests in Nix) and `nix flake check` (to run the derivation-based tests). Both are executed in CI.

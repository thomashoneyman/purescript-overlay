# PureScript Nix

An overlay and flake exposing PureScript tools maintained by the core team (ie. the compiler, `purs`, and package manager, `spago`). Tested on the following architectures:

- x86_64-linux
- x86_64-darwin (Intel Mac)
- aarch64-darwin (M1 Mac)

Via the overlay you can access tooling versions in one of two ways:

- Named executables: The overlay inserts `purs`, `spago`, and `purs-unstable` into your packages. These are tracked in the [named.json](./manifests/named.json) file.
- Versioned executables: The overlay inserts `purs-bin` and `spago-bin` attrsets into your packages, which you can then use to access a specific version of a tool. For example, you can use `purs-bin.purs-0_15_8` to get the `v0.15.8` compiler. These are tracked in the [manifests](./manifests/) directory.

This Nix library also includes helper functions for building PureScript packages, namely:

- `buildSpagoLock`: Discover and build all workspaces and dependencies from a spago.lock file
- `buildPackageLock`: Discover and download dependencies listed in a package-lock.json (:warning: only suitable for simple projects like typical PureScript FFI; for significant applications there are better solutions like npmlock2nix)

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
            pkgs.spago-bin.spago-0_93_4
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

## Development

This repository exports functions from [nix/lib](./nix/lib/) and packages listed in the [manifests](./manifests/) directory.

The [overlay.nix](./overlay.nix) file auto-discovers what tools to build and export via the manifests directory. Each manifest contains the arguments necessary to build a derivation for the relevant tool and is produced by the `generate` script (see below).

- `purs.json`: Contains a mapping of systems to the tarball name and hash as can be found in the PureScript GitHub releases
- `spago.json`: Contains the commit hash for the commit to build Spago with.

There is a generation script stored in the [generate](./generate/) directory which will search for new versions of the tools included in this repository and attempt to produce new derivations for them. This script is run in CI nightly, but can also be run manually:

```console
nix run .#generate
```

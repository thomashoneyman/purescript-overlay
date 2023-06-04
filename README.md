# PureScript Nix

An overlay and flake exposing PureScript tools maintained by the core team (ie. the compiler, `purs`, and package manager, `spago`). Tested on the following architectures:

- x86_64-linux
- x86_64-darwin (Intel Mac)
- aarch64-darwin (M1 Mac)

This Nix library also includes helper functions for building PureScript packages, namely:

- `buildSpagoLock`: Discover and build all workspaces and dependencies from a spago.lock file
- `buildPackageLock`: Discover and download dependencies listed in a package-lock.json (:warning: only suitable for simple projects like typical PureScript FFI; for significant applications there are better solutions like npmlock2nix)

There is an example project in the [`example`](./example) repository which demonstrates how to package a PureScript application using multiple Spago workspaces and foreign Node dependencies into an executable via the utilities in this library. Specifically, see the included [`default.nix`](./example/default.nix).

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
            pkgs.purs.purs-unstable
            pkgs.spago.spago-0_93_4
          ];
        };
      }
    );
}
```

You can also run individual packages from the flake, e.g.

```sh
nix run github:thomashoneyman/purescript-nix#purs
nix run github:thomashoneyman/purescript-nix#purs-unstable
nix run github:thomashoneyman/purescript-nix#spago
nix run github:thomashoneyman/purescript-nix#spago-0_93_4
```

# PureScript Overlay

[![CI](https://github.com/thomashoneyman/purescript-overlay/actions/workflows/ci.yaml/badge.svg)](https://github.com/thomashoneyman/purescript-overlay/actions/workflows/ci.yaml)
[![daily-update](https://github.com/thomashoneyman/purescript-overlay/actions/workflows/daily-update.yaml/badge.svg)](https://github.com/thomashoneyman/purescript-overlay/actions/workflows/daily-update.yaml)

Pure and reproducible Nix overlay for the standard PureScript toolchain. The toolchain is auto-updated every day. Currently supported tools:

- `purs`, the compiler
- `spago`, the package manager
- `purs-tidy`, the code formatter
- `purs-backend-es`, the optimizer
- `purescript-language-server`, the language server protocol

> :warning: This library is unstable and may be reorganized. Use at your own risk!

The overlay is tested on the following architectures:

- x86_64-linux
- x86_64-darwin (Intel Mac)
- aarch64-darwin (M1 Mac)
- aarch64-linux

The included overlay inserts the latest stable and unstable executables into your packages (ie. `purs`, `purs-unstable`, and so on). You can see all specific versions in the [named.json](./manifests/named.json) file. It also provides many versions of each executable under a `-bin` namespace (ie. `purs-bin`, `spago-bin`, and so on) so you can access specific versions of a tool. For example, you can use `purs-bin.purs-0_15_8` to get the 0.15.8 PureScript compiler. These are tracked in the [manifests](./manifests/) directory.

If you're looking for a way to *build* your `spago`-based projects using Nix, take a look at https://github.com/jeslie0/mkSpagoDerivation.

## Usage

In a Nix flake, apply the overlay when importing nixpkgs to get access to tools like `purs` and `spago`. For example, the below flake creates a development shell with the PureScript compiler, Spago, and a pinned version of `purs-tidy`:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, purescript-overlay, ... }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (
          system:
          f (
            import nixpkgs {
              inherit system;
              overlays = [ purescript-overlay.overlays.default ];
            }
          )
        );
    in
    {
      # pkgs now has access to the standard PureScript toolchain
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            purs
            spago
            purs-tidy-bin.purs-tidy-0_10_0
            purs-backend-es
            purescript-language-server
          ];
        };
      });
    };
}
```

The latest versions of all the supported tools are included in the default development shell of this flake. To enter a `nix develop` shell environment with all of the supported tools:

```console
nix develop github:thomashoneyman/purescript-overlay
```

You can also run individual packages from the flake, e.g. to format your `src` directory:

```console
nix run github:thomashoneyman/purescript-overlay#purs-tidy format-in-place src
```

You can run a `nix shell` with `purs` and `spago-unstable` selected from this flake:

```console
nix shell github:thomashoneyman/purescript-overlay#purs github:thomashoneyman/purescript-overlay#spago-unstable
```

## Tests

Run all checks locally:

```bash
# Enter the dev shell
nix develop

# Run generator tests
cd generate
npm ci
spago test

# Verify manifests can be read correctly
spago run -p bin -- verify ../manifests

# Use --verbose for detailed output (shows GitHub API calls, hashes, etc.)
spago run -p bin -- --verbose verify ../manifests

# Check Nix formatting and package derivations
cd ..
nix flake check
```

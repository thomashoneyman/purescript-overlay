# PureScript Nix

An overlay and flake exposing PureScript tools maintained by the core team (ie. the compiler, `purs`, and package manager, `spago`). Tested on the following architectures:

- x86_64-linux
- x86_64-darwin (Intel Mac)
- aarch64-darwin (M1 Mac)

This Nix library also includes helper functions for building PureScript packages, namely:

- `buildSpagoLock`: Discover and build all workspaces and dependencies from a spago.lock file
- `buildPackageLock`: Discover and download dependencies listed in a package-lock.json (:warning: only suitable for simple projects like typical PureScript FFI; for significant applications there are better solutions like npmlock2nix)

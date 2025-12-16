# Generate

This directory holds a PureScript script that checks for new releases for tools supported in this repository and generates Nix files for them. It's intended to be run as a GitHub Action on a daily basis.

## Development

Enter the development shell from the repository root:

```console
nix develop
```

Then run commands from this directory using `spago run`.

## Commands

Supported commands:

### Verify

Given a path to the manifests directory, verifies that the tool can properly read the manifests.

```console
spago run -p bin -- verify ../manifests
```

### Prefetch

Given a path to the manifests directory, looks up recent releases for the supported tools and prints releases that do not appear in the manifests. Does not actually write any information.

```console
spago run -p bin -- prefetch ../manifests
```

### Update

Given a path to the manifests directory, looks up recent releases for the supported tools and creates new manifests for releases that don't already appear in the list.

```console
spago run -p bin -- update ../manifests
```

Optionally takes a flag `--commit` which will commit the changes and open a pull request against this repository. This command is only available to repository collaborators.

```console
spago run -p bin -- update ../manifests --commit
```

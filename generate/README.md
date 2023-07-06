# Generate

This directory holds a PureScript script that checks for new releases for tools supported in this repository and generates Nix files for them. It's intended to be run as a GitHub Action on a daily basis.

## Commands

Supported commands:

### Verify

Given a path to the manifests directory, verifies that the tool can properly read the manifests.

```console
nix run .#generate verify ../manifests
```

### Prefetch

Given a path to the manifests directory, looks up recent releases for the supported tools and prints releases that do not appear in the manifests. Does not actually write any information.

```console
nix run .#generate prefetch ../manifests
```

### Update

Given a path to the manifests directory, looks up recent releases for the supported tools and creates new manifests for releases that don't already appear in the list.

```console
nix run .#generate update ../manifests
```

Optionally takes a flag `--commit` which will commit the changes and open a pull request against this repository. This command is only available to repository collaborators.

```console
nix run .#generate -- update ../manifests --commit
```

## Tools

The `purs` compiler is supported at versions going back to 0.13.0. It is built by fetching the tarballs the compiler team uploads to GitHub releases.

The `spago` build tool is supported for all versions implemented in PureScript (as of the time of writing, that's 0.93.4). It is built by associating a GitHub release with a commit SHA, then fetching the repo at that SHA and building its contents using `buildSpagoLock`.

#### Upcoming

I plan to support the rest of the standard PureScript build toolchain, namely `purs-tidy` and `purs-backend-es`. Both will be fetched the same way as Spago, except that (as of the time of writing) they do not have `spago.yaml` files. For this reason, the `generate` script will need to clone these packages, use spago@0.21.0 to run the `migrate` command, then switch to the new Spago to generate a lockfile with `--generate-lockfile`. Then, it can persist the generated `spago.yaml` and `spago.lock` files in a directory in this project so they can be used to build the project source.

Once these packages switch to the new-style Spago then this workaround will not be needed anymore.

## Development Notes

- If you want to open a pull request then you will need to either set the REPO_TOKEN environment variable to a GitHub token with access to the purescript-nix repository or create a .env file that sets REPO_TOKEN=valid-token. If you need to change the token then make sure to update the repository secrets as well.

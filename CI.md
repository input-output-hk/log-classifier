# Buildkite & Nix Builds

## Buildkite
The buildkite pipeline is automatically triggered by any commits that are pushed to the log-classifier repo.
The pipeline that is executed is located in the .buildkite folder.


## Nix related files and their purposes

| File | Description |
| --- | --- |
| default.nix | Contains the primary nix expression used to build log-classifier. This is the only nix expression that a developer may need to modify. |
| cabal2nix.nix | The cabal2nix generated output from the log-classifier.cabal file. |
| fetch-nixpkgs.nix | Used for pinning a specific version of nixpkgs. Reads nixpkgs-src.json. |
| fetchNixpkgs.nix | Used for pinning a specific version of nixpkgs. Verifies and pulls nixpkgs. |
| lib.nix | Used for pinning a specific verison of nixpkgs. Checks for cardano sl pkgs and runs fetch-nixpkgs.nix otherwise |
| shell.nix | Used by nix-shell to set nix environment. |
| universum.nix | Created by cabal2nix, essentially overrides the Universum in nixpkgs. |
| nixpkgs-src.json | Where version of nixpkgs is defined. See below for instructions to update. |

## Building via Nix

These are the instructions for using nix to build log-classifier.

All the commands below are executed in the cloned log-classifier directory.

```
$ nix build -f default.nix
```

You can also build within nix-shell: 

```
$ nix-shell
nix-shell $ runhaskell Setup.hs configure
nix-shell $ runhaskell Setup.hs build
```

If you would prefer to use the repl:
```
$ nix-env -f '<nixpkgs>' -iA haskellPackages.ghcid
$ nix-shell
nix-shell $ ghcid -c "runhaskell Setup.hs repl Lib" # or replace log-classifier-exe if preferred
```

### If log-classifier.cabal file is modified

Execute the following:

```
$ cabal2nix . > cabal2nix.nix
```

### If Universum version needs to be changed

```
$ cabal2nix cabal://universum-1.1.0 > universum.nix
```

### To change version of nixpkgs

Get $REV from https://howoldis.herokuapp.com/

```
$ REV="d7d31fea7e7eef8ff4495e75be5dcbb37fb215d0"
$ nix-prefetch-url https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz
$ nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz
```
Copy the outputs into the respective entries in nixpkgs-src.json

### To get versions of haskell packages
```
nix-shell --run 'ghc-pkg list' | grep universum
```

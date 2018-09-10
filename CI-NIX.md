# Buildkite & Nix Builds

## Buildkite
The buildkite pipeline is automatically triggered by any commits that are pushed to the log-classifier repo.
The pipeline that is executed is located in the .buildkite folder.


## Nix related files and their purposes

| File | How to Make | Description |
| --- | --- | --- |
| fetch-nixpkgs.nix | [cardano sl](https://github.com/input-output-hk/cardano-sl) | Used for pinning a specific version of nixpkgs. Reads nixpkgs-src.json. |
| fetchNixpkgs.nix | [cardano sl](https://github.com/input-output-hk/cardano-sl) | Used for pinning a specific version of nixpkgs. Verifies and pulls nixpkgs. |
| shell.nix | Manually | Used by nix-shell to set nix environment. |
| release.nix | Manually | Used by Hydra |
| nixpkgs-src.json | See Below | Where version of nixpkgs is defined. See below for instructions to update. |

## Developing / Building via Nix

Note: We are using stack for local builds again. Nix is being used via the CI, but only to run stack and weeder.

All the commands below are executed in the cloned log-classifier directory.

Build with test output:
```
$ nix-build
```

You can also build within nix-shell: 

```
$ nix-shell
nix-shell $ runhaskell Setup.hs configure
nix-shell $ runhaskell Setup.hs build
nix-shell $ runhaskell Setup.hs test
```

If you would prefer to use the repl:
```
$ nix-env -f '<nixpkgs>' -iA haskellPackages.ghcid
$ nix-shell
nix-shell $ ghcid -c "runhaskell Setup.hs repl Lib" # or replace log-classifier-exe if preferred
```

### To change version of nixpkgs

Get $REV from https://howoldis.herokuapp.com/ (click commit, copy the string next to commit)

```
$ REV="d7d31fea7e7eef8ff4495e75be5dcbb37fb215d0"
$ cat ./nixpkgs-src.json | jq ".rev = \"${REV}\"" | \
jq ".sha256 = \"`nix-prefetch-url https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz`\"" | \
jq ".sha256unpacked = \"`nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz`\""
```

### To get versions of haskell packages
```
nix-shell --run 'ghc-pkg list' | grep universum
```

## Useful Nix Resources

- [Nix by example](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)
- [Nix Cheatsheet](https://nixos.wiki/wiki/Cheatsheet)
- [Pinning Nixpkgs](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)
- [Nix Overrides](https://nixos.org/nixpkgs/manual/#sec-overrides)
- [How I Develop with Nix](https://ocharles.org.uk/posts/2014-02-04-how-i-develop-with-nixos.html)

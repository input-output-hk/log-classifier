# Buildkite & Nix Builds

## Buildkite
The buildkite pipeline is automatically triggered by any commits that are pushed to the log-classifier repo.
The pipeline that is executed is located in the .buildkite folder.

## Nix

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
nix-shell $ ghcid -c "runhaskell Setup.hs repl Lib"
```

## To change version of nixpkgs
Get $REV from https://howoldis.herokuapp.com/
```
$ REV="d7d31fea7e7eef8ff4495e75be5dcbb37fb215d0"
$ SHA=`nix-prefetch-url https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz`
$ SHA-UNPACK=`nix-prefetch-url --unpack https://github.com/nixos/nixpkgs/archive/${REV}.tar.gz`
$ source nix-pkgs.sh > nix-pkgs.json
```
nix-pkgs-sh
```
echo '
{
    "rev":            "$REV",
    "sha256":         "$SHA",
    "sha256unpacked": "$SHA-UNPACK"
}'

```

## To get versions
```
nix-shell --run 'ghc-pkg list' | grep universum
```

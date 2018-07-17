# Buildkite

These are the instructions for generating nix expressions for buildkite CI for log-classifier.

```
$ cd $LOG_CLASSIFIER_ROOT
$ cabal2nix . > default.nix
$ nix-shell -p nix-prefetch-git
nix-shell $ nix-prefetch-git https://github.com/NixOS/nixpkgs.git > nixpkgs.json
```

NOTE: 
  - NIX_PATH - Get from https://github.com/NixOS/nixpkgs, click clone, copy ZIP link, rename to .tar.gz.
  - buildkite pipeline command is "nix-build -I nixpkgs=$NIX_PATH release1.nix"

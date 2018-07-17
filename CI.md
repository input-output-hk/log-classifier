# Buildkite
```
$ nix-shell -p nix-prefetch-git

$ nix-prefetch-git https://github.com/NixOS/nixpkgs.git > nixpkgs.json
 
```

cabal2nix . > default.nix

NOTE: 
  - NIX_PATH - Get from https://github.com/NixOS/nixpkgs, click clone, copy ZIP link, rename to .tar.gz.
  - buildkite pipeline  is now: nix-build -I nixpkgs=$NIX_PATH release1.nix

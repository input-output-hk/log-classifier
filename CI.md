# Buildkite
```
$ nix-shell -p nix-prefetch-git

$ nix-prefetch-git https://github.com/NixOS/nixpkgs.git > nixpkgs.json
 
```


.buildkite/pipeline.yml command: 
get NIX_PATH from https://github.com/NixOS/nixpkgs, click clone, copy ZIP link, rename to .tar.gz.
  nix-build -I nixpkgs=$NIX_PATH release1.nix

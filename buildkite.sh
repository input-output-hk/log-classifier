#! /bin/bash

# curl -sSL https://get.haskellstack.org/ | sh
nix-shell -p bash --command "stack build"

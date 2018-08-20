#! /bin/bash

# curl -sSL https://get.haskellstack.org/ | sh

nix-shell --command "stack test"

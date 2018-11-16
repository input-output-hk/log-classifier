# TODO: release.nix is broken

let
  fixedLib     = import ./lib.nix;
  fixedNixpkgs = fixedLib.iohkNix.nixpkgs;
in
  { supportedSystems ? [ "x86_64-linux" ]
  , scrubJobs ? true
  , fasterBuild ? false
  , skipPackages ? []
  , nixpkgsArgs ? {
      config = { allowUnfree = false; inHydra = true; };
    }
  }:

with (import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

let
  pkgs = import fixedNixpkgs { config = {}; };
  haskellPackages = map (name: lib.nameValuePair name supportedSystems) fixedLib.logClassiferPkgList;

  platforms = {
    log-classifier-web = supportedSystems;
    log-classifier = supportedSystems;
  };
  mapped = mapTestOn platforms;
  makeTestRuns = system:
  let
    pred = name: value: (fixedLib.isLogClassifier name);
    f = name: value: value.testrun;
    logClassiferPkgs = import ./. { inherit system; };
  in pkgs.lib.mapAttrs f (lib.filterAttrs pred logClassiferPkgs.haskellPackages);

in pkgs.lib.fix (jobsets: mapped // {
  inherit (pkgs) cabal2nix;
  nixpkgs = fixedNixpkgs;
  # the result of running every cardano test-suite on 64bit linux
  log-classifier-tests.x86_64-linux = makeTestRuns "x86_64-linux";
  # hydra will create a special aggregate job, that relies on all of these sub-jobs passing
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "log-classifer-required-checks";
    constituents =
      let
        allLinux = x: map (system: x.${system}) [ "x86_64-linux" ];
        all = x: map (system: x.${system}) supportedSystems;
      in
    [
      (builtins.concatLists (map lib.attrValues (all jobsets.log-classifier-tests)))
    ];
  });
})


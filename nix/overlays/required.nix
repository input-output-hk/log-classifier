{ pkgs }:

with import ../../lib.nix;

with pkgs.haskell.lib;

self: super: rec {
        universum           = pkgs.haskell.lib.dontCheck (self.callHackage "universum" "1.1.0" {});
        # because of http://hackage.haskell.org/package/concurrent-output
        concurrent-output   = pkgs.haskell.lib.dontCheck (self.callPackage ../../concurrent-output.nix {});
        stm                 = pkgs.haskell.lib.dontCheck (self.callHackage "stm" "2.4.5.1" {});

        # LIB
        log-classifier      = self.callCabal2nix "log-classifier" ../../classifier { };
        log-classifier-web  = self.callCabal2nix "web" ../../web { };
}

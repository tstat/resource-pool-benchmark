{
  description = "Basic haskell";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let
      compiler = "ghc8104";
      pkgs = let
        p0 = nixpkgs.legacyPackages.x86_64-linux;
        exts = p0.lib.composeManyExtensions [ self.overlays.haskellOverlay ];
      in p0.extend exts;
      ghc = pkgs.haskell.packages."${compiler}";
    in {
      overlays = {
        haskellOverlay = final: prev: {
          haskell = with prev.haskell.lib;
            prev.haskell // {
              packages = let
                ghcs = prev.lib.filterAttrs
                  (k: v: prev.lib.strings.hasPrefix "ghc" k)
                  prev.haskell.packages;
                patchedGhcs = builtins.mapAttrs patchGhc ghcs;
                patchGhc = k: v:
                  prev.haskell.packages."${k}".extend (self: super:
                    with prev.haskell.lib;
                    with builtins;
                    with prev.lib.strings;
                    let
                      cleanSource = pth:
                        let
                          src' = prev.lib.cleanSourceWith {
                            filter = filt;
                            src = pth;
                          };
                          filt = path: type:
                            let isHiddenFile = hasPrefix "." (baseNameOf path);
                            in !isHiddenFile;
                        in src';
                    in {
                      resource-pool-waiter-patch = let
                        psrc = builtins.fetchGit {
                          url = "https://github.com/tstat/pool";
                          rev = "243a5b365b26775ea68ec138e18172662135d036";
                        };
                        p = self.callCabal2nix "resource-pool-waiter-patch" psrc
                          { };
                        g = drv: {
                          patches = [ ./resource-pool.patch ];
                        };
                      in overrideCabal p g;
                      resource-pool-benchmark = let
                        p = self.callCabal2nix "resource-pool-benchmark"
                          (cleanSource ./src) { };
                      in p;
                    });
              in prev.haskell.packages // patchedGhcs;
            };
        };
      };

      apps.x86_64-linux.repl = flake-utils.lib.mkApp {
        drv = nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };

      devShell.x86_64-linux = ghc.shellFor {
        withHoogle = false;
        packages = hpkgs:
          with hpkgs;
          with pkgs.haskell.lib;
          let
            p = overrideCabal resource-pool-benchmark (drv: {
              doBenchmark = true;
              # I don't know why doBenchmark is insufficient, but it
              # doesn't add the bench deps to the shell.
              libraryHaskellDepends = drv.libraryHaskellDepends
                ++ drv.benchmarkHaskellDepends;
            });
          in [ p ];
      };

      pkgs = pkgs;

      packages.x86_64-linux = { };

      defaultPackage.x86_64-linux = self.devShell.x86_64-linux;
    };
}

{ compiler ? "ghc821" }:

let
  bootstrap = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: rec {
              icicli = self.callPackage ./default.nix {};
              icicli-static =
                pkgs.haskell.lib.overrideCabal
                  (self.callPackage ./default.nix {})
                  (oldDerivation: { enableSharedExecutables = false; });
            };
          };
        };
      };
    };
  };
  pkgs = import src { inherit config; };
in
{ icicli = pkgs.haskell.packages.${compiler}.icicli;
  icicli-static = pkgs.haskell.packages.${compiler}.icicli-static;
}

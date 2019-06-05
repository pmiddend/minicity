{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, brick, cabal-install, containers
      , hindent, hlint, microlens, microlens-ghc, microlens-th
      , pretty-simple, stdenv, text, vty
      }:
      mkDerivation {
        pname = "minicity";
        version = "1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base brick containers microlens microlens-ghc microlens-th
          pretty-simple text vty
        ];
        executableToolDepends = [ cabal-install hindent hlint ];
        homepage = "https://github.com/pmiddend/minicity";
        description = "Build a city, keep it in balance";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

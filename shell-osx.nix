{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, hakyll, hfsevents, hxt
      , stdenv, time, time-locale-compat
      }:
      mkDerivation {
        pname = "mikey-bike";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableSystemDepends = [
          pkgs.darwin.apple_sdk.frameworks.Cocoa
        ];
        executableHaskellDepends = [
          base filepath hakyll hfsevents hxt time time-locale-compat
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

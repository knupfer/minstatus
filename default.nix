{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell.lib.compose) overrideCabal;
  
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  
  f = { mkDerivation, base, bytestring, lib, process, time }:
      mkDerivation {
        pname = "minstatus";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base bytestring process time ];
        description = "Minimal status command for sway or i3";
        license = lib.licenses.bsd3;
        mainProgram = "minstatus";
      };
 
  drv = haskellPackages.callPackage f {};
  drv-inc-out = overrideCabal (drv: {
    doInstallIntermediates = true;
    enableSeparateIntermediatesOutput = true;
  }) drv;
  drv-inc = overrideCabal (drv: {
    previousIntermediates = drv-inc-out.intermediates;
  }) drv;
  
in

  if pkgs.lib.inNixShell then drv-inc.env else drv-inc

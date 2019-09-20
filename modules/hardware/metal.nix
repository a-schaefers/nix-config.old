{ config, pkgs, lib, ... }:
# Enable for all machines with real hardware (e.g. NOT virtual machines.)
with lib;
{
  options.modules.hardware.metal.enable = mkEnableOption "modules.hardware";
  config = mkIf config.modules.hardware.metal.enable {

    boot.kernelModules = [ "coretemp" ];

    services.smartd.enable = true;

    services.haveged.enable = true;

    environment.systemPackages = with pkgs; [
      pmutils lm_sensors smartmontools
    ];

  };
}

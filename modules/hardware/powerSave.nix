{ config, pkgs, lib, ... }:
# generic laptop power savings settings.
with lib;
{
  options.modules.hardware.powerSave.enable = mkEnableOption "modules.hardware.power.laptops";
  config = mkIf config.modules.hardware.powerSave.enable {

    powerManagement.enable = true;
    services.tlp.enable = true;

    environment.systemPackages = with pkgs; [ acpi ];

  };
}

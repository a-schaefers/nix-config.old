{ config, pkgs, lib, ... }:
# generic desktop power settings
with lib;
{
options.modules.hardware.power.enable = mkEnableOption "modules.hardware.power.desktops";
config = mkIf config.modules.hardware.power.enable {

powerManagement = {
enable = true;
cpuFreqGovernor = "performance";
};

};
}

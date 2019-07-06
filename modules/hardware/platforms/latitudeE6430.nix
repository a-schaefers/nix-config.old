{ config, pkgs, lib, ... }:
# generic dell latitude E6430 configuration
with lib;
with import ../../../util;
{
options.modules.hardware.platform.latitudeE6430.enable = mkEnableOption "hardware.platform.latitudeE6430";
config = mkIf config.modules.hardware.platform.latitudeE6430.enable {

modules.hardware = enableMultiple [
"metal"
"powerSave"
"intel-sna"
# "intel-modesetting"
"touchpad" # my touchpad broke
"intel-microcode"
];

services.xserver.libinput.accelSpeed = "0.75";
};
}

{ config, pkgs, lib, ... }:
# generic HP Z620 configuration
with lib;
with import ../../../../util;
{
options.modules.hardware.platform.hpZ620.enable = mkEnableOption "hardware.platform.hpZ620";
config = mkIf config.modules.hardware.platform.hpZ620.enable {

modules.hardware = enableMultiple [
"metal"
"power"
"intel-microcode"
];

};
}

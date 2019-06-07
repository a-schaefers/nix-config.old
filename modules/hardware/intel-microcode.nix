{ config, pkgs, lib, ... }:
with lib;
{
options.modules.hardware.intel-microcode.enable = mkEnableOption "modules.intel-microcode";
config = mkIf config.modules.hardware.intel-microcode.enable {
hardware.cpu.intel.updateMicrocode = true;
};
}

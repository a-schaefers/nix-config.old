{ config, pkgs, lib, ... }:
with lib;
{
options.modules.hardware.amd-microcode.enable = mkEnableOption "modules.amd-microcode";
config = mkIf config.modules.hardware.amd-microcode.enable {
hardware.cpu.amd.updateMicrocode = true;
};
}

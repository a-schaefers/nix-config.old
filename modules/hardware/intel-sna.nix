{ config, pkgs, lib, ... }:
with lib;
{
options.modules.hardware.intel-sna.enable = mkEnableOption "modules.hardware.intel-sna";
config = mkIf config.modules.hardware.intel-sna.enable {

boot.initrd.kernelModules = [ "i915" ];

# services.xserver.videoDrivers = [ "modesetting" ];
services.xserver.videoDrivers = [ "intel" ];
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "sna"
'';

hardware.opengl.extraPackages = with pkgs;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];
hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];

environment.sessionVariables = {
LIBVA_DRIVER_NAME = "i965";
VDPAU_DRIVER = "va_gl";
};

hardware.brightnessctl.enable = true;
environment.systemPackages = with pkgs; [
brightnessctl
];

};
}

{ config, pkgs, lib, ... }:
with lib;
{
options.modules.hardware.intelgfx.enable = mkEnableOption "modules.hardware.intelgfx";
config = mkIf config.modules.hardware.intelgfx.enable {

boot.kernelParams = [ "i915.enable_fbc=1" ];
boot.initrd.kernelModules = [ "i915" ];

services.xserver.videoDrivers = [ "modesetting" ];
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';

hardware.opengl.extraPackages = with pkgs;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];
hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];

environment.sessionVariables = {
LIBVA_DRIVER_NAME = "i965";
VDPAU_DRIVER = "va_gl";
};

programs.light.enable = true;
};
}

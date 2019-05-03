{ config, pkgs, lib, ... }:
with lib;
{
options.modules.hardware.amdgpu.enable = mkEnableOption "modules.hardware.amdgpu";
config = mkIf config.modules.hardware.amdgpu.enable {

boot.kernelParams = [ "amdgpu.dc=1" ];
boot.initrd.kernelModules = [ "amdgpu" ];

services.xserver.videoDrivers = [ "modesetting" ];

services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';

hardware.opengl.extraPackages = with pkgs;
[ libvdpau-va-gl vaapiVdpau ];
hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
[ libvdpau-va-gl vaapiVdpau ];
};
}

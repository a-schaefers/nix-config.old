{ config, pkgs, lib, ... }:
with lib;
{
  options.modules.hardware.intel-modesetting.enable = mkEnableOption "modules.hardware.intel-modesetting";
  config = mkIf config.modules.hardware.intel-modesetting.enable {

    boot.initrd.kernelModules = [ "i915" ];

    services.xserver.videoDrivers = [ "modesetting" ];

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

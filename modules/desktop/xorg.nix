{ config, pkgs, lib, ... }:
with lib;
{
  options.modules.desktop.xorg.enable = mkEnableOption "modules.desktop";
  config = mkIf config.modules.desktop.xorg.enable {

    # Xorg
    services.xserver = {
      xkbOptions = "ctrl:swap_lalt_lctl, caps:menu";
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;
      enable = true;
      layout = "us";
      useGlamor = true;
    };

  };
}

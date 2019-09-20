{ config, pkgs, lib, ... }:

with lib;

{
  options.modules.hardware.touchpad.enable = mkEnableOption "modules.hardware.touchpad";
  config = mkIf config.modules.hardware.touchpad.enable {

    services.xserver.libinput = {
      enable = true;
      tapping = true;
      disableWhileTyping = true;
      scrollMethod = "twofinger";
      naturalScrolling = false;
    };

  };
}

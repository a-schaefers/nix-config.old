{ config, pkgs, lib, ... }:
with lib;
with import ../../util;
{
  options.modules.desktop.enable = mkEnableOption "modules.desktop";
  config = mkIf config.modules.desktop.enable {

    # components of my desktop setup
    modules.desktop = enableMultiple [
      "pulse"
      "xorg"
      "opengl"
      # "gaming"
      "sddm"
      "home"
    ];

  };
}

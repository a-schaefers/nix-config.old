{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.xorg.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.xorg.enable {

# Xorg
services.xserver = {
enable = true;
layout = "us";
useGlamor = true;
};

};
}

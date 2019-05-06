{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.plasma.enable = mkEnableOption "modules.desktop.plasma";
config = mkIf config.modules.desktop.plasma.enable {

services.xserver.desktopManager.plasma5.enable = true;

services.redshift = {
enable = true;
latitude = "43.3665";
longitude = "-124.2179";
};


};
}

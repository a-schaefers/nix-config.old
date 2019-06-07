{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.sddm.enable = mkEnableOption "modules.desktop.sddm";
config = mkIf config.modules.desktop.sddm.enable {

services.xserver = {
desktopManager = {
xterm.enable = false;
};
displayManager = {
sddm.enable = true;
sddm.autoLogin.enable = true;
sddm.autoLogin.relogin = true;
sddm.autoLogin.user = "adam";
};
};

};
}

{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.dm.enable = mkEnableOption "modules.desktop.dm";
config = mkIf config.modules.desktop.dm.enable {

services.xserver = {

desktopManager = {
xterm.enable = false;
};

displayManager = {
sddm.enable = true;
sddm.autoLogin.enable = true;
sddm.autoLogin.relogin = false;
sddm.autoLogin.user = "adam";

};
};
};
}

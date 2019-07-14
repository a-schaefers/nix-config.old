{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.sddm.enable = mkEnableOption "modules.desktop.sddm";
config = mkIf config.modules.desktop.sddm.enable {

services.xserver = {
displayManager = {
sddm.enable = true;
sddm.autoLogin.enable = true;
sddm.autoLogin.relogin = false;
sddm.autoLogin.user = "adam";
};
desktopManager = {
xterm.enable = false;
default = "xsession";
session = [ {
manage = "desktop";
name = "xsession";
start = ''
# edit .xsession file via xsession.windowManager.command in home.nix
'';
} ];
};
};
};
}

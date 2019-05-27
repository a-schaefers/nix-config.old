{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.plasma.enable = mkEnableOption "modules.desktop.plasma";
config = mkIf config.modules.desktop.plasma.enable {

services.xserver.desktopManager.plasma5.enable = true;

environment.systemPackages = with pkgs; [
redshift-plasma-applet

kgpg k3b kate kcalc k3b akonadi latte-dock

vlc phonon phonon-backend-vlc
];

};
}

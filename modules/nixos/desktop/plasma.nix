{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.plasma.enable = mkEnableOption "modules.desktop.plasma";
config = mkIf config.modules.desktop.plasma.enable {

services.xserver.desktopManager.plasma5.enable = true;

environment.systemPackages = with pkgs; [
kgpg k3b kate kcalc k3b akonadi latte-dock
];

};
}

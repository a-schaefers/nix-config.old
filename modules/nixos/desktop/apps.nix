{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

ffmpeg mpv youtube-dl vlc

kgpg k3b kate kcalc k3b akonadi

libreoffice gimp krita inkscape kdenlive blender

google-chrome thunderbird konversation spotify

unetbootin wimlib

redshift redshift-plasma-applet geoclue2
];

};
}

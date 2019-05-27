{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
geoclue2

glxinfo libva-utils vdpauinfo

ffmpeg  mpv youtube-dl

libreoffice gimp

google-chrome spotify
];

};
}

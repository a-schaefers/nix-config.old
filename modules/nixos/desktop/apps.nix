{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

ffmpeg mpv youtube-dl

libreoffice gimp

gnome3.gnome-themes-standard gnome3.adwaita-icon-theme
];

programs.firejail = {
enable = true;
wrappedBinaries = {
chromium = "${lib.getBin pkgs.chromium}/bin/chromium";
};
};

};
}

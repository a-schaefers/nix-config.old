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
];

programs.firejail = {
enable = true;
wrappedBinaries = {
chromium = "${lib.getBin pkgs.chromium}/bin/chromium";
};
};

};
}

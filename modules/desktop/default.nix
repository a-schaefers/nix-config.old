{ config, pkgs, lib, ... }:
with lib;
with import ../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

# don't keep logs for more than a week
services.journald.extraConfig = ''
MaxRetentionSec=7day
'';

# windows 10 dual-boot compat
time.hardwareClockInLocalTime = true;

# alsa
sound.enable = true;

# fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
noto-fonts
liberation_ttf
hack-font
fantasque-sans-mono
terminus_font
];

# apps
environment.systemPackages = with pkgs; [
gnome3.gnome-themes-standard gnome3.gnome-themes-extra gnome3.adwaita-icon-theme
glxinfo libva-utils vdpauinfo
ffmpeg phonon-backend-vlc vlc mpv youtube-dl
];

programs.firejail = {
enable = true;
wrappedBinaries = {
brave = "${lib.getBin pkgs.brave}/bin/brave";
thunderbird = "${lib.getBin pkgs.thunderbird}/bin/thunderbird";
};
};

# components of my desktop setup
modules.desktop = enableMultiple [
"dev"
"pulse"
"xorg"
"opengl"
"sddm"
"exwm"
];

};
}

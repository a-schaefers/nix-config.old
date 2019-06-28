{ config, pkgs, lib, ... }:
with lib;
with import ../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

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

ffmpeg gstreamer vlc
];

programs.firejail = {
enable = true;
wrappedBinaries = {
# chromium = "${lib.getBin pkgs.chromium}/bin/chromium";
google-chrome = "${lib.getBin pkgs.google-chrome}/bin/google-chrome-stable";
thunderbird = "${lib.getBin pkgs.thunderbird}/bin/thunderbird";
mpv = "${lib.getBin pkgs.mpv}/bin/mpv";
youtube-dl= "${lib.getBin pkgs.youtube-dl}/bin/youtube-dl";
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

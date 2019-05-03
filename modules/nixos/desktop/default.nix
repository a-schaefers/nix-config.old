{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

# alsa
sound.enable = true;

# cups
# services.printing.enable = true;

# polkit
security.polkit.enable = true;

# some basic fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
noto-fonts
noto-fonts-cjk
noto-fonts-emoji
liberation_ttf
];

# components of my desktop setup
modules.desktop = enableMultiple [
"apps"
"dev"
"pulse"
"xorg"
"opengl"
"dm"
"exwm"
];

};
}

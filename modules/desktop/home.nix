{ config, pkgs, lib, ... }:
with lib;
let
my-dotfile-dir = "/nix-config/dotfiles";
home-manager = builtins.fetchGit {
url = "https://github.com/rycee/home-manager.git";
ref = "master";
};
in
{

imports = [
"${home-manager}/nixos"
];

options.modules.desktop.home.enable = mkEnableOption "modules.desktop.home";
config = mkIf config.modules.desktop.home.enable {

# GLOBAL SECTION

# don't keep desktop logs for more than a day
services.journald.extraConfig = ''
MaxRetentionSec=1day
'';

# fonts
fonts.fonts = with pkgs; [
liberation_ttf
dejavu_fonts
source-code-pro
noto-fonts
hack-font
fantasque-sans-mono
terminus_font
];

environment.systemPackages = with pkgs; [
# themes
gnome3.gnome-themes-standard gnome3.gnome-themes-extra gnome3.adwaita-icon-theme hicolor-icon-theme

# window-manager helpers
wmctrl xclip xsel scrot imagemagick libnotify perlPackages.FileMimeInfo

# multimedia players/libs
glxinfo libva-utils vdpauinfo
ffmpeg phonon-backend-vlc vlc mpv youtube-dl

# apps
gimp qbittorrent
];

# firejail high-risk packages
programs.firejail = {
enable = true;
wrappedBinaries = {
brave = "${lib.getBin pkgs.brave}/bin/brave";
thunderbird = "${lib.getBin pkgs.thunderbird}/bin/thunderbird";
};
};

# screen lock
programs.slock.enable = true;
programs.xss-lock.enable = true;
programs.xss-lock.lockerCommand = "/run/wrappers/bin/slock";

# disk mount
services.udisks2.enable = true;

# .bashrc
programs.bash.interactiveShellInit = ''
emacs_dumb_term() {
    export PAGER="cat"
    export TERM="xterm-256color"
    man () { /usr/bin/man "$@" | col -bx ; }
    grep -q "nixos" /etc/issue && man () { /run/current-system/sw/bin/man "$@" | col -bx ; }
    watch() { while true ; do "$@" ; sleep 2;  echo ; done }
}
[[ "$TERM" = dumb ]] && [[ "$INSIDE_EMACS" ]] && emacs_dumb_term

'';

# .bash_profile
programs.bash.loginShellInit = ''
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
'';

# BEGIN HOME-MANAGER USER SECTIONS

services.dbus.packages = with pkgs; [ gnome3.dconf ]; # required by gtk / home-manager

home-manager.users.adam = {

# pkgs
home.packages = [
];

programs.home-manager = {
enable = true;
path = "…";
};

# themes
xsession.pointerCursor = {
package = pkgs.gnome3.gnome-themes-standard;
size = 16; # default = 32; example = 64;
defaultCursor = "left_ptr"; # example = "X_cursor";
name = "Adwaita";
};
gtk = {
enable = true;
font = {
name = "Hack 13";
package = pkgs.hack-font;
};
iconTheme = {
name = "Adwaita";
package = pkgs.gnome3.adwaita-icon-theme;
};
theme = {
name = "Adwaita-dark";
package = pkgs.gnome3.gnome-themes-standard;
};
};
qt = {
enable = true;
platformTheme = "gnome"; # gnome or gtk
};

# notifications
services.dunst = {
enable = true;
settings = {
global = {
font = "Hack 13";
frame_color = "#000000";
separator_color = "#000000";
};
urgency_normal = {
background = "#000000";
foreground = "#4870a1";
};
};
};

# git
programs.git = {
enable = true;
userName  = "Adam Schaefers";
userEmail = "paxchristi888@gmail.com";
};

# symlinks for programs for which Home Manager doesn't have configuration options
home.file.".mailcap".source = "${my-dotfile-dir}/.mailcap";

#FIXME
# home.activation.linkEmacsCustom = config.lib.dag.entryAfter [ "writeBoundary" ] ''
# mkdir -p $HOME/.emacs.d
# ln -sf ${my-dotfile-dir}/* $HOME/.emacs.d
# '';

}; # end home-manager.users.adam section

};
}

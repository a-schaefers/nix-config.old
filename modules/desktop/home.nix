{ config, pkgs, lib, ... }:
with lib;
let
dmesg-notify = pkgs.writeScriptBin "dmesg-notify" ''
#!/usr/bin/env python

import os
import time

def executeCommand(the_command):
    temp_list = os.popen(the_command).read()
    return temp_list

def getDMESG():
    return executeCommand("dmesg | tail -n 1")

def compareStatus(current_status):
    temp_var=getDMESG()
    if (temp_var!=current_status):
        current_status=temp_var
        os.system("notify-send \"" + current_status + "\"")
    time.sleep(2)
    return current_status

def main():
    current_status=getDMESG()
    while (2<3):
        current_status=compareStatus(current_status)

if __name__ == "__main__":
    main()
'';
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
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

# exwm
(emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
epkgs.xelb
epkgs.exwm
epkgs.desktop-environment
epkgs.pdf-tools
epkgs.emms
epkgs.hydra
epkgs.transpose-frame

epkgs.better-defaults
epkgs.magit
epkgs.projectile
epkgs.flycheck
epkgs.crux
epkgs.ace-window
epkgs.webpaste
epkgs.aggressive-indent
epkgs.dumb-jump
epkgs.rich-minority
epkgs.sexy-monochrome-theme
epkgs.diff-hl
epkgs.browse-kill-ring

epkgs.bash-completion
epkgs.xterm-color
epkgs.elisp-slime-nav
epkgs.paredit
epkgs.rainbow-delimiters
epkgs.rainbow-mode
epkgs.clojure-mode
epkgs.cider
epkgs.nix-mode
epkgs.haskell-mode
])))
gnupg pinentry gnutls (python36.withPackages(ps: with ps; [ certifi ]))
redshift networkmanagerapplet volumeicon udiskie dmesg-notify
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
platformTheme = "gtk"; # gnome or gtk
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
urgency_low = {
background = "#000000";
foreground = "#4870a1";
};
urgency_normal = {
background = "#000000";
foreground = "#4870a1";
};
urgency_critical = {
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
home.file.".emacs.d/init.el".source = "${my-dotfile-dir}/.emacs.d/init.el";
home.file.".emacs.d/modules".source = "${my-dotfile-dir}/.emacs.d/modules";
home.file.".mailcap".source = "${my-dotfile-dir}/.mailcap";
home.file.".config/mimeapps.list".source = "${my-dotfile-dir}/.config/mimeapps.list";
home.file.".local/share/applications/emacsclient-usercreated-1.desktop".source = "${my-dotfile-dir}/.local/share/applications/emacsclient-usercreated-1.desktop";

xsession.enable = true;
xsession.windowManager.command = ''
xset +dpms
xset s 1800
xset dpms 0 0 1860
dmesg-notify &
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
emacs
trap 'kill $(jobs -p)' EXIT
'';

home.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
XDG_CURRENT_DESKTOP = "EXWM";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

}; # end home-manager.users.adam section

};
}

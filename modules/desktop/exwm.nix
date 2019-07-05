{ config, pkgs, lib, ... }:
with lib;
let

myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

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

my-dots = pkgs.writeScriptBin "my-dotfiles" ''
mkdir -p "$HOME"/{Downloads,Pictures,Documents}

mkdir "$HOME/.emacs.d"
ln -sf "/nix-config/dotfiles/.emacs.d"/* "$HOME/.emacs.d"

mkdir -p "$HOME/.config"
cat << EOF > "$HOME/.config/mimeapps.list"
[Default Applications]
application/pdf=emacsclient-usercreated-1.desktop;
inode/directory=emacsclient-usercreated-1.desktop;
inode/mount-point=emacsclient-usercreated-1.desktop;
text/html=brave-browser.desktop
x-scheme-handler/http=brave-browser.desktop
x-scheme-handler/https=brave-browser.desktop
x-scheme-handler/about=brave-browser.desktop
x-scheme-handler/unknown=brave-browser.desktop
EOF

mkdir -p "$HOME/.local/share/applications"
cat << EOF > "$HOME/.local/share/applications/emacsclient-usercreated-1.desktop"
[Desktop Entry]
Version=1.0
Encoding=UTF-8
Type=Application
Name=emacsclient
NoDisplay=true
Exec=emacsclient
EOF
'';
in
{

options.modules.desktop.exwm.enable = mkEnableOption "modules.desktop.exwm";
config = mkIf config.modules.desktop.exwm.enable {

services.xserver.desktopManager = {
default = "emacs";
session = [ {
manage = "desktop";
name = "emacs";
start = ''
xset +dpms
xset s 1800
xset dpms 0 0 1860
my-dotfiles
dmesg-notify &
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
emacs &
waitPID=$!
trap 'kill $(jobs -p)' EXIT
'';
} ];
};

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
XDG_CURRENT_DESKTOP = "EXWM";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

environment.systemPackages = with pkgs; [
my-dots dmesg-notify

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
redshift networkmanagerapplet volumeicon udiskie
];

};
}

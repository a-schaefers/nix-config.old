{ config, pkgs, lib, callPackage, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

myDots = pkgs.writeScriptBin "myDots" ''
mkdir -p "$HOME"/{Downloads,Pictures,Documents}

mkdir "$HOME/.emacs.d"
ln -sf "/nix-config/dotfiles/.emacs.d"/* "$HOME/.emacs.d"

cat << EOF > "$HOME/.gitconfig"
[user]
name = Adam Schaefers
email = paxchristi888@gmail.com
EOF

mkdir -p "$HOME/.config"
cat << EOF > "$HOME/.config/mimeapps.list"
[Default Applications]
application/pdf=emacsclient-usercreated-1.desktop;
inode/directory=emacsclient-usercreated-1.desktop;
inode/mount-point=emacsclient-usercreated-1.desktop;
text/html=google-chrome.desktop
x-scheme-handler/http=google-chrome.desktop
x-scheme-handler/https=google-chrome.desktop
x-scheme-handler/about=google-chrome.desktop
x-scheme-handler/unknown=google-chrome.desktop
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

cat << EOF > "$HOME/.mailcap"
application/pdf; emacsclient %s
image/png; emacsclient %s
image/jpeg; emacsclient %s
image/gif; emacsclient %s
EOF

mkdir -p "$HOME/.config/dunst"
cat << EOF > "$HOME/.config/dunst/dunstrc"
frame_color = "#000000"
separator_color = "#000000"
[my_low]
msg_urgency = low
background = "#000000"
foreground = "#4870a1"
[my_normal]
msg_urgency = normal
background = "#000000"
foreground = "#4870a1"
[my_critical]
msg_urgency = critical
background = "#000000"
foreground = "#4870a1"
EOF

cat << EOF > "$HOME/.Xresources"
Xcursor.theme: Adwaita
EOF

mkdir -p "$HOME/.icons/default"
cat << EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=Adwaita
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
stupid-power-manager &
myDots
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
myDots

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
wmctrl xclip xsel scrot imagemagick
udiskie libnotify dunst perlPackages.FileMimeInfo
redshift networkmanagerapplet volumeicon
];

programs.slock.enable = true;
programs.xss-lock.enable = true;
programs.xss-lock.lockerCommand = "/run/wrappers/bin/slock";

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
};
}

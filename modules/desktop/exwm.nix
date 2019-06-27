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

mkdir -p "$HOME/.config/mpv"
cat << EOF > "$HOME/.config/mpv/mpv.conf"
profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample
x11-bypass-compositor=yes
EOF

mkdir -p "$HOME/.config/gtk-3.0"
cat << EOF > "$HOME/.config/gtk-3.0/settings.ini"
[Settings]
gtk-theme-name=Adwaita-dark
gtk-icon-theme-name=Adwaita
gtk-font-name=Hack 18
gtk-cursor-theme-name=Adwaita
gtk-cursor-theme-size=0
gtk-toolbar-style=GTK_TOOLBAR_TEXT
gtk-toolbar-icon-size=GTK_ICON_SIZE_MENU
gtk-button-images=0
gtk-menu-images=0
gtk-enable-event-sounds=0
gtk-enable-input-feedback-sounds=0
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle=hintslight
gtk-xft-rgba=rgb
EOF

mkdir -p "$HOME/.config/dunst"
cat << EOF > "$HOME/.config/dunst/dunstrc
frame_color = "#000000"
separator_color = "#000000"
[my_low]
msg_urgency = low
background = "#3a3432"
foreground = "#ffffff"
[my_normal]
msg_urgency = normal
background = "#4a4543"
foreground = "#ffffff"
[my_critical]
msg_urgency = critical
background = "#db2d20"
foreground = "#ffffff"
[global]
monitor = 0
follow = mouse
geometry = "300x5-30+20"
indicate_hidden = yes
shrink = no
transparency = 0
notification_height = 0
separator_height = 2
padding = 8
horizontal_padding = 8
frame_width = 0
sort = yes
idle_threshold = 120
font = Hack 18
line_height = 0
markup = full
format = "<b>%s</b>\n%b"
alignment = left
show_age_threshold = 60
word_wrap = yes
ellipsize = middle
ignore_newline = no
stack_duplicates = true
hide_duplicate_count = false
show_indicators = yes
icon_position = off
max_icon_size = 32
#icon_path = /usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/
sticky_history = yes
history_length = 20
#dmenu = /usr/bin/dmenu -p dunst:
#browser = /usr/bin/firefox -new-tab
always_run_script = true
title = Dunst
class = Dunst
startup_notification = false
verbosity = mesg
corner_radius = 0
EOF

cat << EOF > "$HOME/.gtkrc-2.0"
gtk-theme-name="Adwaita-dark"
gtk-icon-theme-name="Adwaita"
gtk-font-name="Hack 18"
gtk-cursor-theme-name="Adwaita"
gtk-cursor-theme-size=0
gtk-toolbar-style=GTK_TOOLBAR_TEXT
gtk-toolbar-icon-size=GTK_ICON_SIZE_MENU
gtk-button-images=0
gtk-menu-images=0
gtk-enable-event-sounds=0
gtk-enable-input-feedback-sounds=0
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle="hintslight"
gtk-xft-rgba="rgb"
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

epkgs.xelb
epkgs.exwm
epkgs.desktop-environment
epkgs.edit-server
epkgs.pdf-tools
epkgs.emms
epkgs.transpose-frame
epkgs.hydra

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

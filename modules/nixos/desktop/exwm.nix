{ config, pkgs, lib, callPackage, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
myDots = pkgs.writeScriptBin "myDots" ''
cat << EOF > "$HOME/.gitconfig"
[user]
name = Adam Schaefers
email = paxchristi888@gmail.com
EOF

cat << EOF > "$HOME/.mailcap"
application/pdf; emacsclient %s
image/png; emacsclient %s
image/jpeg; emacsclient %s
image/gif; emacsclient %s
EOF

[ ! -d "$HOME/.config/mimi" ] && mkdir -p "$HOME/.config/mimi"
cat << EOF > "$HOME/.config/mimi/mime.conf"
text/: emacsclient
application/pdf: emacsclient
image/: emacsclient
audio/: mpv
video/: mpv
EOF

[ ! -d "$HOME/.config/mpv" ] && mkdir -p "$HOME/.config/mpv"
cat << EOF > "$HOME/.config/mpv/mpv.conf"
profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample
x11-bypass-compositor=yes
EOF

dotfiles="/nix-config/dotfiles"
ln -sf "$dotfiles/"* "$HOME"
[ ! -d "$HOME/org" ] && mkdir -p "$HOME/org"
touch "$HOME/org"/{bookmarks,calendar,clock,todo}.org
[ ! -d "$HOME/.emacs.d/" ] &&  mkdir "$HOME/.emacs.d"
ln -sf "$dotfiles/.emacs.d/"* "$HOME/.emacs.d"

[ ! -d "$HOME/.config/gtk-3.0" ] && mkdir -p "$HOME/.config/gtk-3.0"
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

[ ! -d "$HOME/.icons/default" ] && mkdir -p "$HOME/.icons/default"
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
[ -f "$HOME/.autostart" ] && /bin/sh ~/.autostart
${myDots}/bin/myDots
${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources
${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
${myEmacs}/bin/emacs &
waitPID=$!
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
myEmacs gnupg pinentry gnutls (python36.withPackages(ps: with ps; [ certifi ]))
wmctrl xclip xsel scrot
redshift geoclue2 networkmanagerapplet volumeicon
];

};
}

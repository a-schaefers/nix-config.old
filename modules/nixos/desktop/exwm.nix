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
[ ! -d "$HOME/.emacs.d/" ] &&  mkdir "$HOME/.emacs.d"
ln -sf "$dotfiles/.emacs.d/"* "$HOME/.emacs.d"


[ ! -d "$HOME/.config/gtk-3.0" ] && mkdir -p "$HOME/.config/gtk-3.0"
cat << EOF > "$HOME/.config/gtk-3.0/settings.ini"
[Settings]
gtk-cursor-theme-name=left_ptr
EOF

cat << EOF > "$HOME/.gtkrc-2.0"
gtk-cursor-theme-name="left_ptr"
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
myDots myEmacs wmctrl xclip xsel scrot
];

services.redshift = {
enable = true;
latitude = "43.3665";
longitude = "-124.2179";
temperature.night = 2000;
};

services.compton = {
enable = true;
backend = "glx";
fade = true;
};

services.xserver.displayManager.setupCommands = ''
${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
'';

};
}

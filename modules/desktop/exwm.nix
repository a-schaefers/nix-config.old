{ config, pkgs, lib, callPackage, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

stupid-power-manager = pkgs.writeScriptBin "stupid-power-manager" ''
die() {
    [ $# -gt 0 ] && printf -- "%s\n" "(SPM) $*"
    exit 1
}

[ "$(pidof -o %PPID -x "''${0##*/}")" ] && die "Stupid Power Manager is already running"

batt_thresholds="99 80 40 20 10 5"

[ ! -d "$HOME/.config/stupid-power-manager/state" ] && \
    mkdir -p "$HOME/.config/stupid-power-manager/state"
[ ! -f "$HOME/.config/stupid-power-manager/config" ] && \
    die "Please create $HOME/.config/stupid-power-manager/config and try again."

rm -f "$HOME/.config/stupid-power-manager/state"/* > /dev/null 2>&1 #shh

while true; do
    . "$HOME/.config/stupid-power-manager/config"
    batt_status="$(cat /sys/class/power_supply/BAT0/status)"
    batt_percent="$(cat /sys/class/power_supply/BAT0/capacity)"
    if [ "$batt_status" = "Discharging" ]; then
        echo "$batt_thresholds" | tr ' ' '\n' | while read -r state; do
            if [ "$batt_percent" -eq "$state" ] || [ "$batt_percent" -lt "$state" ]; then
                if [ ! -f "$HOME/.config/stupid-power-manager/state/$state" ]; then
                    rm -f "$HOME/.config/stupid-power-manager/state/100"
                    touch "$HOME/.config/stupid-power-manager/state/$state"
                    user_custom_low_battery_hook
                fi
            fi
        done
    fi
    if [ "$batt_status" = "Charging" ] || [ "$batt_status" = "Full" ]; then
        if [ ! -f "$HOME/.config/stupid-power-manager/state/100" ]; then
            rm -f "$HOME/.config/stupid-power-manager/state"/* > /dev/null 2>&1 #shh
            touch "$HOME/.config/stupid-power-manager/state/100"
            user_custom_battery_normal_hook
        fi
    fi
    sleep 2
done
'';

my-dots = pkgs.writeScriptBin "my-dotfiles" ''
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

cat << EOF > "$HOME/.mailcap"
application/pdf; emacsclient %s
image/png; emacsclient %s
image/jpeg; emacsclient %s
image/gif; emacsclient %s
EOF

mkdir -p "$HOME/.config/dunst"
cat << EOF > "$HOME/.config/dunst/dunstrc"
[global]
font = "Hack 13"
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

cat << EOF > "$HOME/.gtkrc-2.0"
gtk-theme-name="Adwaita-dark"
gtk-icon-theme-name="Adwaita"
gtk-font-name="Hack 13"
gtk-cursor-theme-name="Adwaita"
EOF

mkdir -p "$HOME/.config/gtk-3.0"
cat << EOF > "$HOME/.config/gtk-3.0/settings.ini"
[Settings]
gtk-theme-name=Adwaita-dark
gtk-icon-theme-name=Adwaita
gtk-font-name=Hack 13
gtk-cursor-theme-name=Adwaita
EOF

cat << EOF > "$HOME/.Xresources"
Xcursor.theme: Adwaita
EOF

mkdir -p "$HOME/.icons/default"
cat << EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=Adwaita
EOF

mkdir -p "$HOME/.config/stupid-power-manager/state"
cat << EOF > "$HOME/.config/stupid-power-manager/config"
# example desktop notification command
notifycmd() { emacsclient -e '(battery)' }

# example backlight commands
# backlightcmd="xbacklight -set"
# backlightcmd="light -S"
backlightcmd="brightnessctl s" ; optPercent="%"

user_custom_low_battery_hook() {
    if [ "\$batt_percent" -lt 100 ] && [ "\$batt_percent" -gt 80 ]; then
        # 80-99% batt_percentery
        notifycmd
        \$backlightcmd 100\$optPercent

     elif [ "\$batt_percent" -lt 81 ] && [ "\$batt_percent" -gt 40 ]; then
        # 40-80% battery
        notifycmd
        \$backlightcmd 80\$optPercent

     elif [ "\$batt_percent" -lt 41 ] && [ "\$batt_percent" -gt 20 ]; then
        # 20-40% battery
        notifycmd
        \$backlightcmd 40\$optPercent

     elif [ "\$batt_percent" -lt 21 ] && [ "\$batt_percent" -gt 10 ]; then
        # 10-20% battery
        notifycmd
        \$backlightcmd 20\$optPercent

     elif [ "\$batt_percent" -lt 11 ] && [ "\$batt_percent" -gt 5 ]; then
        # 5-10% battery
        notifycmd
        backlight_low

     elif [ "\$batt_percent" -lt 6 ]; then
        # 5% battery or less
        notifycmd
        \$backlightcmd 5\$optPercent
        systemctl suspend
    fi
}

user_custom_battery_normal_hook() {
    # battery Charging / Full
    notifycmd
    \$backlightcmd 100\$optPercent
}
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
stupid-power-manager &
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
my-dots stupid-power-manager

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
udiskie perlPackages.FileMimeInfo
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

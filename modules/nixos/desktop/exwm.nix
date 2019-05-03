{ config, pkgs, lib, callPackage, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.exwm.enable = mkEnableOption "modules.desktop.exwm";
config = mkIf config.modules.desktop.exwm.enable {

services.xserver.desktopManager = {
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
myEmacs keychain

# xorg / minimal wm helpers
compton wmctrl scrot feh xorg.setxkbmap xorg.xset xorg.xrdb xclip xsel
];

};
}

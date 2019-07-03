{ config, pkgs, lib, ... }:
with lib;
let
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

services.dbus.packages = with pkgs; [ gnome3.dconf ]; # required for gtk themes

home-manager.users.adam = { # begin my user section

xsession.pointerCursor = {
package = pkgs.vanilla-dmz;
name = "Vanilla-DMZ";
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
platformTheme = "gtk";
};

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

programs.git = {
enable = true;
userName  = "Adam Schaefers";
userEmail = "paxchristi888@gmail.com";
};

}; # end home-manager.users.adam section

};
}

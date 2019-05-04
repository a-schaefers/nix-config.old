{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.console.enable = mkEnableOption "modules.desktop.console";
config = mkIf config.modules.desktop.console.enable {

# start to console instead of display-manager
services.xserver.autorun = false;
services.mingetty.autologinUser = "root";
# Some more help text.
services.mingetty.helpLine =
''
The "root" account has an empty password.  ${
optionalString config.services.xserver.enable
"Type `systemctl start display-manager' or `dm' to start the graphical user interface."}
'';

};
}

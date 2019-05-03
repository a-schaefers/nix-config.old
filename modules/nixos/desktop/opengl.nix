{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.opengl.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.opengl.enable {

hardware.opengl = {
driSupport = true;
driSupport32Bit = true;
};

};
}

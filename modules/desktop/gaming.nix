{ config, pkgs, lib, callPackage, ... }:
with lib;
{
options.modules.desktop.gaming.enable = mkEnableOption "modules.desktop.gaming";
config = mkIf config.modules.desktop.gaming.enable {

hardware.opengl.driSupport32Bit = true;
hardware.pulseaudio.support32Bit = true;

environment.systemPackages = with pkgs; [
steam
];

};
}

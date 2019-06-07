{ config, pkgs, lib, callPackage, ... }:
with lib;
{
options.modules.desktop.pulse.enable = mkEnableOption "modules.desktop.pulse";
config = mkIf config.modules.desktop.pulse.enable {

nixpkgs.config.pulseaudio = true;

hardware.pulseaudio.enable = true;
hardware.pulseaudio.support32Bit = true;

environment.systemPackages = with pkgs; [ pulseaudioFull pavucontrol ];

};
}

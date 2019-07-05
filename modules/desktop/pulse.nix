{ config, pkgs, lib, callPackage, ... }:
with lib;
{
options.modules.desktop.pulse.enable = mkEnableOption "modules.desktop.pulse";
config = mkIf config.modules.desktop.pulse.enable {

# alsa
sound.enable = true;

# pulse
nixpkgs.config.pulseaudio = true;
hardware.pulseaudio.enable = true;
hardware.pulseaudio.support32Bit = true;

#pkgs
environment.systemPackages = with pkgs; [ pulseaudioFull pavucontrol ];

};
}

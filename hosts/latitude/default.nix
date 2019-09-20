{ config, pkgs, ... }:
# Dell Latitude E6430 - Top Level Nixfile
{
  imports = [ ../../modules ];

  # Computer name
  networking.hostName = "latitude";

  # use a generic latitudeE6430 profile
  modules.hardware.platform.latitudeE6430.enable = true;

  # Enable the modular desktop profile
  modules.desktop.enable = true;

  # Services
  modules.services.libvirtd.enable = true;
}

{ config, pkgs, ... }:
# HP Z620 - Top Level Nixfile
{
imports = [
../../modules
./pci-passthrough.nix
./quirks.nix
];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "hpz";

# Use a generic hpZ620 profile
modules.hardware.platform.hpZ620.enable = true;

# Use a generic amdgpu profile
modules.hardware.amdgpu.enable = true;

# Enable the modular desktop profile
modules.desktop.enable = true;

# Services
system.autoUpgrade.enable = true;
modules.services.libvirtd.enable = true;
}

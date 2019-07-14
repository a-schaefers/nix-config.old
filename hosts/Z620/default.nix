{ config, pkgs, ... }:
# HP Z620 - Top Level Nixfile
{
imports = [
../../modules
./quirks.nix
];

system.stateVersion = "19.09";

# Computer name
networking.hostName = "Z620";

# Use a generic hpZ620 profile
modules.hardware.platform.hpZ620.enable = true;

# Use a generic amdgpu profile
modules.hardware.amdgpu.enable = true;

# Enable the modular desktop profile
modules.desktop.enable = true;

# Experimental
system.autoUpgrade.enable = true;
boot.zfs.enableUnstable = true;
boot.zfs.requestEncryptionCredentials = true;
}

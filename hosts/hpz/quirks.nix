{ config, pkgs, ... }:
# misc machine-unique settings that don't precisely fit into my various modules.
{
# use until 4.15 or greater becomes nixos default.
# boot.kernelPackages = pkgs.linuxPackages_4_17;

# amdgpu.audio parameter needed by amd rx460
boot.kernelParams = [ "amdgpu.audio=0" ];
}

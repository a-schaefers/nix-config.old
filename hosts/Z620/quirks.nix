{ config, pkgs, ... }:
# misc machine-unique settings that don't precisely fit into my various modules.
{
# amdgpu.audio parameter needed by amd rx460
boot.kernelParams = [ "amdgpu.audio=0" ];
}

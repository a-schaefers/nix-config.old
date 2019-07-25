{ config, pkgs, ... }:
# misc machine-unique settings that don't precisely fit into my various modules.
{

boot.kernelParams = [
"amdgpu.audio=0" # amdgpu.audio parameter needed by amd rx460
];
}

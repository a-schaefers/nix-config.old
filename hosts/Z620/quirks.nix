{ config, pkgs, ... }:
# misc machine-unique settings that don't precisely fit into my various modules.
{

boot.kernelParams = [
"amdgpu.audio=0" # amdgpu.audio parameter needed by amd rx460
"memmap=64K$0" "memory_corruption_check=0" # https://bbs.archlinux.org/viewtopic.php?id=189483
];
}

{ config, pkgs, ... }:
{

documentation = {
info.enable = true;
man.enable = true;
};

programs = {
less.enable = true;
mtr.enable = true;
bash.enableCompletion = true;
};

environment.systemPackages = with pkgs; [
nix-prefetch-scripts
nixops
nix-index
coreutils
pciutils
gptfdisk
dosfstools
unzip
zip
psmisc
lsof
htop
iotop
powertop
tree
pstree
ltrace
strace
linuxPackages.perf
wget
tmux
cryptsetup
parted
gparted
syslinux
efibootmgr
unetbootin
wimlib
bind
openvpn
];

}

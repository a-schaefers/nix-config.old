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
nix-prefetch-scripts nixops nix-index
coreutils pciutils
parted
gptfdisk
dosfstools
unzip
zip
lsof tree pstree psmisc
ltrace strace linuxPackages.perf
wget
cryptsetup
efibootmgr
bind
inotify-tools
gitAndTools.gitFull
emacs
file
];

}

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
    emacs
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
    file
  ];

}

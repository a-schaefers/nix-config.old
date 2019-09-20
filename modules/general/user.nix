{ config, pkgs, ... }:
{

  security.sudo.wheelNeedsPassword = true;

  nix.allowedUsers = [ "root" "@wheel" ];
  nix.trustedUsers = [ "root" "@wheel" ];

  users.users.adam = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "disk"
      "audio"
      "video"
      "systemd-journal"
      "networkmanager"
    ];
    initialPassword = "password";
  };
}

# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=libvirt-iso.nix
{config, pkgs, ...}:
let
  themelios = pkgs.writeScriptBin "themelios" ''
    bash <(curl https://raw.githubusercontent.com/a-schaefers/themelios/master/themelios) $@
  '';
in {
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];
  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDVukS7izRS8xtTgAGcqi0UceqWV2EU/Fj9Z7cvfOwqrxMY0ffuyvvqE3Xez/CVuM+1QY/QECBUZjuurG7G2SubkHsH9j+n5b9fdSx5mzZ/jvzplSluJn/jrv88EmnMGwGv4/ylKi6FVFHhOUGWLu8cfISEe/6ZhZFWANFUSpSfXssvLVjDritazdIf8KEvZoFDw7AX+xf1YJ87WJA8ZENbsWhmI5U6nPat4rVIp4bgBcoMtukaktDdGWWxhbJLIaTJ+xHXHZ0yG+qzqg9kEF4KL1X3/sJdjKA7IvrRStK/aSiN3bXFfIA9WX5tFgJETDC4GEE9KdoMVEi3Fw9v3XbF adam@zbox"
  ];
  networking = {
    usePredictableInterfaceNames = false;
    interfaces.eth0.ip4 = [{
      address = "192.168.122.99";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.122.1";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall.allowPing = true;
    firewall.allowedTCPPorts = [ 22 ];
    firewall.allowedUDPPorts = [ 22 ];
  };
  boot.supportedFilesystems = [ "zfs" ];
  environment.systemPackages = with pkgs; [ themelios git ];
}

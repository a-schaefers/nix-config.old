{config, pkgs, ...}:
# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=libvirtiso.nix
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
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz6zBuWvmgyvkpKqQcUdl7Wt48mvwPtWr+QWwjAF9dLUTJK6Qb7PcDUCjzpeM09cE7rqCXx+dleyxDZXao+ctS6/wgPGdQvcNukSjKxmrmNt407hnyNEIrNhUeHCYhN/cBuuANx90yuJRP6e8cQMIgNBaT6MhNN2ipYy157v++iN5Rm+gEp1CSPRNL9cp7guLGV9VT4T1URJO/b9XE43ca2Y2G6UhZqSAI1NON4jENzw6WW5QmaWNXHJOyKb5ArIHnM4QuyEz8dAfo3oK+l4VQfale0VDlK9k2ugrriLvOaQZt6a756e52cRPc+1r6yQO+YEVvZEGLYl/1cqQwE10OGpUCyFnctrVcot1OCsFnmNmZwgV95yyWAMg8ajACarm1W5Bs4rTLs7UhIgrnkpcAlPsuuWOxpCx9ws3dFMbnbp8O1G/uhKMn0PEotX6ZuAVH40hsMErTRHqnxl6t2rPhiS9sqq6ERFWM8Rci3gSEs+PnTDr5aJ3FGOZ4BeWqVAd4F9V5S1XHXGy4G8vh4Nn2/H7ZxhgHi+F07M0Mt/G8PgUON7qzRcq8V8kxNLpx4uTfNZaWmQWLNZ/hP0ieq++VULYaMCp6tq3kjBX+UKJD0CmBxz89JPyOMALZWoG/sKSasp2JNfdfwhoGS1Djixch3AM66hlT3f7dnmHvH5yE9Q=="
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
  environment.systemPackages = with pkgs; [ themelios git emacs ];
}

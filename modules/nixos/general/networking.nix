{ config, pkgs, ... }:
{
services.openssh.enable = false;
services.openssh.permitRootLogin = "yes";

networking = {

networkmanager = {
enable = true;
};

firewall = {
allowedTCPPorts = [ 22 ];
allowedUDPPorts = [ 22 ];
allowPing = true;
};

nameservers = [ "8.8.8.8" "8.8.4.4" ];
};

}

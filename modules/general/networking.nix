{ config, pkgs, ... }:
{
services.openssh.enable = true;

networking = {

networkmanager = {
enable = true;
};

firewall = {
allowedTCPPorts = [ 22 ];
allowedUDPPorts = [ 22 ];
allowPing = true;
};

nameservers = [ "1.1.1.1" "1.0.0.1" ];

enableIPv6 = false;

};

}

{ config, pkgs, lib, ... }:
with lib;
{
imports = [  ];

options.modules.services.libvirtd.enable = mkEnableOption "modules.services.libvirtd";
config = mkIf config.modules.services.libvirtd.enable {

users.users.adam.extraGroups = [ "libvirtd" ];

services.dnsmasq.enable = true;
virtualisation.libvirtd.enable = true;
networking.firewall.checkReversePath = false;
environment.variables.LIBVIRT_DEFAULT_URI = "qemu:///system";

environment.systemPackages = with pkgs; [
OVMF
virtmanager
];

programs.dconf.enable = true;  # https://github.com/NixOS/nixpkgs/issues/42433

virtualisation.libvirtd.onShutdown = "shutdown";

virtualisation.libvirtd.qemuVerbatimConfig = ''
nvram = [ "${pkgs.OVMF}/FV/OVMF.fd:${pkgs.OVMF}/FV/OVMF_VARS.fd" ]
'';

};

}

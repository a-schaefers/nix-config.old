# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=myrescueiso.nix
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

networking = {
networkmanager.enable = true;
wireless.enable = false;
nameservers = [ "8.8.8.8" "8.8.4.4" ];
};

boot.supportedFilesystems = [ "zfs" ];
environment.systemPackages = with pkgs; [ git themelios ];
}

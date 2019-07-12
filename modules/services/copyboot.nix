{ config, pkgs, lib, ... }:
# this is a workaround
# UEFI ESP's have a limitation in that they cannot RAID.
# this script monitors /boot for changes; and upon change, copies the files to /boot2
# this keeps the systemd-boot loader in sync for multi-disk arrays.
with lib;
let
copyBootScript = pkgs.writeScriptBin "copyBootScript" ''
while inotifywait -r -e modify,create,delete /boot
do
sleep 2 # for good measure
rm -rf /boot2/* ; cp -a /boot/* /boot2
done
'';
in
{
imports = [  ];

options.modules.services.copy-boot.enable = mkEnableOption "modules.services.copy-boot";
config = mkIf config.modules.services.copy-boot.enable {

systemd.services.copy-boot = {
path = [ pkgs.inotify-tools ];

after = [ "multi-user.target" ];
wantedBy = [ "multi-user.target" ];
script = "${copyBootScript}/bin/copyBootScript";
serviceConfig = {
User = "root";
Restart = "on-failure";
};
};

systemd.services.copy-boot.enable = true;

};
}

{ config, pkgs, lib, ... }:
with lib;
let
copyBootScript = pkgs.writeScriptBin "copyBootScript" ''
while inotifywait -r -e modify,create,delete /boot
do
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
};
};

systemd.services.copy-boot.enable = true;

};
}

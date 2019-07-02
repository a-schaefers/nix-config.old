{ config, pkgs, lib, ... }:
with lib;
{
imports = [  ];

options.modules.services.copyboot.enable = mkEnableOption "modules.services.copyboot";
config = mkIf config.modules.services.copyboot.enable {

systemd.services.copyBoot = {
path = [ pkgs.inotify-tools pkgs.rsync ];

after = [ "multi-user.target" ];
wantedBy = [ "multi-user.target" ];
script = "while inotifywait -r -e modify,create,delete /boot ; do rsync -av --delete /boot/ /boot2 ; done";
serviceConfig = {
User = "root";
};
};

systemd.services.copyBoot.enable = true;

};
}

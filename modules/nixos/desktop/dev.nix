{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.dev.enable = mkEnableOption "modules.desktop.dev";
config = mkIf config.modules.desktop.dev.enable {

environment.systemPackages = with pkgs; [
gnupg pinentry gnutls (python36.withPackages(ps: with ps; [ certifi ]))

gitAndTools.gitFull
shellcheck
ripgrep
ag
];

};
}

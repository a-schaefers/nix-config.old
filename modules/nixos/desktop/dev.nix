{ config, pkgs, lib, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.dev.enable = mkEnableOption "modules.desktop.dev";
config = mkIf config.modules.desktop.dev.enable {

environment.systemPackages = with pkgs; [
myEmacs gnupg pinentry gnutls (python36.withPackages(ps: with ps; [ certifi ]))

# misc
gitAndTools.gitFull
shellcheck
ripgrep
ag
];

};
}

{ config, pkgs, ... }:
{

services.journald.extraConfig = ''
MaxRetentionSec=1month
'';

}

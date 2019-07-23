{ config, pkgs, ... }:
{
nixpkgs.config.allowUnfree = true;
nix.useSandbox = false;
}

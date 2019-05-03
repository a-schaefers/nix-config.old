{ config, pkgs, ... }:
{
nixpkgs.config = {
allowUnfree = true;
};
nix.useSandbox = true;
nix.autoOptimiseStore = true;
}

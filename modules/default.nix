{ lib, ... }: {
  imports = import ../lib/recimport.nix { inherit lib; dir = ./.; };
}

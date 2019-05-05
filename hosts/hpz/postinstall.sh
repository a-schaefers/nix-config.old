# setup dotfiles
cat << EOF | nixos-enter
sudo -u adam bash /nix-config/dotfiles/bin/dotfiles-install.sh
EOF

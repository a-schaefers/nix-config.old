# setup dotfiles
cat << EOF | nixos-enter
chown -R adam:users /nix-config
sudo -u adam bash /nix-config/dotfiles/bin/dotfiles-install.sh
EOF

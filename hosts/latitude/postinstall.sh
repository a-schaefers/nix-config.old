# setup home

cat << EOF | nixos-enter
sudo -u adam mkdir -p ~/{.emacs.d,repos,Downloads,Documents,Pictures,.config}
sudo -u adam ln -s /${nix_repo_name}/dotfiles/{*,.*} /home/adam
sudo -u adam ln -s /${nix_repo_name}/dotfiles/.config/* /home/adam/.config
sudo -u adam ln -s /${nix_repo_name}/dotfiles/.emacs.d/* /home/adam/.emacs.d
EOF

dotfiles="/nix-config/dotfiles"
[ ! -d "$HOME/.emacs.d/" ] &&  mkdir "$HOME/.emacs.d"
ln -sf "$dotfiles/.emacs.d/"* "$HOME/.emacs.d"
ln -sf "$dotfiles" "$HOME"

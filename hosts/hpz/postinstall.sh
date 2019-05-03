# get my keys off my usb stick

mount_usb="/tmp/private"
usb_disk_id="/dev/disk/by-id/usb-PNY_USB_2.0_FD_A0708H32YD34001168-0:0"

mkdir "$mount_usb"
mount "$usb_disk_id" "$mount_usb"
cp -a "${mount_usb}/Private" "/mnt/home/adam"
umount "$mount_usb"

# setup dotfiles

cat << EOF | nixos-enter
sudo -u adam mkdir -p ~/{.emacs.d,repos,Downloads,Documents,Pictures,.config}
sudo -u adam ln -s /${nix_repo_name}/dotfiles/{*,.*} ~/
sudo -u adam ln -s /${nix_repo_name}/dotfiles/.config/* ~/.config
sudo -u adam ln -s /${nix_repo_name}/dotfiles/.emacs.d/* ~/.emacs.d
sudo -u adam mv ~/Private/adam.org ~/
chown -R adam:users /"${nix_repo_name}"
chown -R adam:users /home/adam
EOF

cat << EOF > "$HOME/.xserverrc"
#!/bin/sh
exec /usr/bin/Xorg -nolisten tcp -nolisten local "\$@" vt$XDG_VTNR
EOF

cat << EOF > "$HOME/.mailcap"
application/pdf; emacsclient %s
image/png; emacsclient %s
image/jpeg; emacsclient %s
image/gif; emacsclient %s
EOF

[ ! -d "$HOME/.config/mimi" ] && mkdir -p "$HOME/.config/mimi"
cat << EOF > "$HOME/.config/mimi/mime.conf"
text/: emacsclient
application/pdf: emacsclient
image/: emacsclient
audio/: mpv
video/: mpv
EOF

[ ! -d "$HOME/mpv" ] && mkdir -p "$HOME/.config/mpv"
cat << EOF > "$HOME/.config/mpv/mpv.conf"
profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample

x11-bypass-compositor=yes
EOF

dotfiles="/nix-config/dotfiles"
[ ! -d "$HOME/.emacs.d/" ] &&  mkdir "$HOME/.emacs.d/"
ln -s "$dotfiles/.emacs.d/"* "$HOME/.emacs.d"
ln -s "$dotfiles" "$HOME"

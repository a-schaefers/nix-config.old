#!/bin/sh

# adjust brightness based on power threshholds
simple-power-manager -d

# shut off screen after 5 minutes idle time
xset +dpms
xset dpms 300

[ "$(pidof -o %PPID -x "${0##*/}")" ] && exit # bail if script is already running

# slock screen when screen is off
while true; do
    if xset -q | grep -q "Monitor is Off"; then
        pgrep slock || slock
    fi
    sleep 60
done &

#!/bin/sh

# adjust brightness based on power threshholds
simple-power-manager -d

# poweroff display after 5 idle minutes
xset +dpms
xset dpms 0 0 300

[ "$(pidof -o %PPID -x "${0##*/}")" ] && exit # bail if script is already running

# slock screen when display is off
while true; do
    if xset -q | grep -q "Monitor is Off"; then
        pgrep slock || slock
    fi
    sleep 60
done &

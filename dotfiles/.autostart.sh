#!/bin/sh

simple-power-manager -d

# shut off screen after 5 minutes idle time
xset +dpms
xset dpms 300

# loop control
if pidof -o %PPID -x ".autostart.sh"; then exit; fi

# slock screen after 5-6 minutes of idle time
while true; do
    if [ "$(xprintidle-ng)" -gt 300000 ]; then
        pgrep slock || slock
    fi
    sleep 60
done &

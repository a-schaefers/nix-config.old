#!/bin/sh

# shut off screen after 5 minutes idle time
xset +dpms
xset dpms 300

# make sure only one set of loops are looping
if pidof -o %PPID -x ".autostart.sh"; then exit; fi

# slock screen after 5-6 minutes of idle time
while true; do
    if [ "$(xprintidle-ng)" -gt 300000 ]; then
        pgrep slock || slock
        sleep 60
    fi
done &

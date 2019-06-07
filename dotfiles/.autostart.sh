#!/bin/sh
[ "$(pidof -o %PPID -x "${0##*/}")" ] && exit # bail if script is already running
stupid-power-manager &
xset +dpms
xset s 300
xset dpms 0 0 600

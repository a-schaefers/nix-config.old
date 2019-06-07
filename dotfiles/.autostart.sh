#!/bin/sh
xset +dpms
xset s 300
xset dpms 0 0 600
compton -b --backend glx
stupid-power-manager &

#!/bin/bash

# normal -> turn left(ccw) -> turn right(cw) -> turn invert(half) -> normal

DEVICE=$(xsetwacom list | grep stylus | awk -F"id:" '{print $1}' | sed -e 's/\s*$//g')
STATUS=$(xsetwacom get "$DEVICE" Rotate)
set_status(){
    xsetwacom set "$DEVICE" Rotate $1
}
case $STATUS in
    NONE|none)
	xrandr -o 3 
    set_status 1
	;;
    CW|cw)
	xrandr -o 0
    set_status 0
	;;
    *)
	echo "Unknown result from 'xsetwacom get stylus Rotate'" >&2
	exit 1
	;;
esac
exec /etc/X11/xinit/xmonad/restart_xmonad.sh

#!/bin/sh
killall xmobar
sh /etc/X11/xinit/xmonad/set_wallpaper.sh
xmonad --restart

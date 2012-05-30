#!/bin/sh
xrandr --output VGA-0 --mode 1600x900 --right-of LVDS --preferred --output LVDS --mode 1280x800
nitrogen --restore
xmonad --restart

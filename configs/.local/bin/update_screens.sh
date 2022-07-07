#!/bin/bash

exec >/home/william/udev.out 2>&1

DP_STATUS=$(cat /sys/class/drm/card0-DP-1/status)

export XAUTHORITY=/run/user/1000/gdm/Xauthority
export DISPLAY=:0

if [ $DP_STATUS = "connected" ]
then
  /usr/bin/xrandr
  /usr/bin/xrandr --output HDMI-1 --off --output DP-2 --off --output eDP-1 --mode 1920x1080 --pos 0x360 --rotate normal --output DP-1 --primary --mode 3440x1440 --pos 1920x0 --rotate normal
  /usr/bin/xmodmap /home/william/.xmodmap
  /usr/bin/xset r rate 300 35
else
  /usr/bin/xrandr --output HDMI-1 --off --output DP-1 --off --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-2 --off
fi

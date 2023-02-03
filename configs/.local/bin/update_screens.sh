#!/bin/bash

exec >/home/william/udev.out 2>&1

DP_STATUS=$(cat /sys/class/drm/card1-HDMI-A-2/status)

export XAUTHORITY=/run/user/1000/gdm/Xauthority
export DISPLAY=:1

if [ $DP_STATUS = "connected" ]
then
  /usr/bin/xrandr
  /usr/bin/xrandr --output eDP-1-1 --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-0 --primary --mode 3440x1440 --pos 2560x160 --rotate normal
  /usr/bin/xmodmap /home/william/.xmodmap
  /usr/bin/xset r rate 300 35
else
  /usr/bin/xrandr --output eDP-1-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-0 --off
fi

# /usr/bin/xrandr --fb 6860x3400 --output eDP-1 --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-1-0 --primary --mode 3440x1440 --pos 2560x160 --rotate normal --scale 1.25x1.25 --panning 4300x1800+2560+160

# libnvidia-cfg1-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-common-515/jammy-updates,jammy-updates,jammy-security,jammy-security,now 515.48.07-0ubuntu0.22.04.2 all [installed,automatic]
# libnvidia-compute-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-compute-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 i386 [installed,automatic]
# libnvidia-decode-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-decode-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 i386 [installed,automatic]
# libnvidia-egl-wayland1/jammy,now 1:1.1.9-1.1 amd64 [installed,automatic]
# libnvidia-encode-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-encode-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 i386 [installed,automatic]
# libnvidia-extra-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-fbc1-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-fbc1-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 i386 [installed,automatic]
# libnvidia-gl-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# libnvidia-gl-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 i386 [installed,automatic]
# linux-modules-nvidia-515-5.15.0-41-generic/jammy-updates,jammy-security,now 5.15.0-41.44+1 amd64 [installed,automatic]
# linux-modules-nvidia-515-generic-hwe-22.04/jammy-updates,jammy-security,now 5.15.0-41.44+1 amd64 [installed]
# linux-objects-nvidia-515-5.15.0-40-generic/jammy-updates,now 5.15.0-40.43+1 amd64 [installed,automatic]
# linux-objects-nvidia-515-5.15.0-41-generic/jammy-updates,jammy-security,now 5.15.0-41.44+1 amd64 [installed,automatic]
# linux-signatures-nvidia-5.15.0-40-generic/jammy-updates,now 5.15.0-40.43+1 amd64 [installed,automatic]
# linux-signatures-nvidia-5.15.0-41-generic/jammy-updates,jammy-security,now 5.15.0-41.44+1 amd64 [installed,automatic]
# nvidia-compute-utils-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# nvidia-driver-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed]
# nvidia-kernel-common-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# nvidia-kernel-source-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# nvidia-prime/jammy,jammy,now 0.8.17.1 all [installed,automatic]
# nvidia-settings/jammy,now 510.47.03-0ubuntu1 amd64 [installed,automatic]
# nvidia-utils-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]
# xserver-xorg-video-nvidia-515/jammy-updates,jammy-security,now 515.48.07-0ubuntu0.22.04.2 amd64 [installed,automatic]

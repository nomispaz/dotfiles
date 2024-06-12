#!/usr/bin/env bash

### AUTOSTART PROGRAMS ###
# wl-clipboard-history -t &
dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=qtile &
/usr/lib64/polkit-kde-authentication-agent-1 &
nm-applet --indicator &
dunst &
#kanshi &

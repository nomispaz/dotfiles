#!/usr/bin/env bash

### AUTOSTART PROGRAMS ###
# wl-clipboard-history -t &
systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP &
dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE &
# start authentication agent
# /usr/lib64/polkit-kde-authentication-agent-1 &
#exec kwalletd6
# /usr/lib/pam_kwallet_init --no-startup-id &
/usr/libexec/polkit-gnome-authentication-agent-1 &
sleep 2 &
nm-applet --indicator &
dunst &
# start gammastep indicator for systray
exec gammastep-indicator &
#kanshi &

# wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && dunstctl close-all && dunstify "mic $(wpctl get-volume @DEFAULT_AUDIO_SOURCE@ | grep -q MUTED && echo muted || echo unmuted)"

wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && notify-send -u low "mic $(wpctl get-volume @DEFAULT_AUDIO_SOURCE@ | grep -q MUTED && echo muted || echo unmuted)"

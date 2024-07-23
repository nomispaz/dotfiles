#! /bin/sh
osname=$(grep '^NAME=' /etc/os-release | cut -d '=' -f 2 | tr -d '"')
if [ $osname == "NixOS" ]; then
	$HOME/git_repos/dotfiles/sway/scripts/autotiling/main_nixos.py
else
	$HOME/git_repos/dotfiles/sway/scripts/autotiling/main.py
fi

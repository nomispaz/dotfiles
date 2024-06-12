#!/bin/bash
cd ~/dotfiles
# loop through all folders and files
for program in $(ls -d  *)
do
  # create softlink to config folder for all folders and files unless it is the README
  if [ ! $program == 'README.md' ]; then
    ln -s ~/dotfiles/$program ~/.config/$program
  fi
done

# make some scripts executable
chmod +x ~/dotfiles/sway/scripts/autotiling/main.py

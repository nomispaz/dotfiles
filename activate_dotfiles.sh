#!/bin/bash
cd ~/dotfiles
# loop through all folders and files
for program in $(ls -d  *)
do
  if [ ! $program == 'README.md' ]; then
    ln -s ~/dotfiles/$program ~/.config/$program
  fi
done

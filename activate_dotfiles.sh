#!/bin/bash
cd ~/dotfiles
for program in $(ls -d  *)
do
  if [ ! -f $program ]; then
    ln -s ~/dotfiles/$program ~/.config/$program
  fi
done


# If not running interactively, don't do anything
[ -z "$PS1" ] && return

if [ -f ~/.fresh/build/shell.sh ]; then
    source ~/.fresh/build/shell.sh
fi

# disable Ctrl-S - but only for interactive shells
if [[ $- == *i* ]]; then
    stty -ixon -ixoff 2>/dev/null
fi

#!/bin/sh
if [ "$TERM" = "linux" ]; then
  /bin/echo -e "
  \e]P0262a26
  \e]P1ff3255
  \e]P200ff00
  \e]P3c3c322
  \e]P47e97fb
  \e]P5ffff00
  \e]P61999b3
  \e]P7808080
  \e]P8687d68
  \e]P9e6193c
  \e]PA29a329
  \e]PBc3c322
  \e]PC3d62f5
  \e]PDad2bee
  \e]PE1999b3
  \e]PFffffff
  "
  # get rid of artifacts
  clear
fi

export EDITOR=emacs
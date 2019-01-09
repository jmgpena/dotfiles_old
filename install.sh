#!/bin/bash

linkDotfile(){
    if [ ! -e $2 ] || [ -h $2 ]
    then
        echo "Linking '$1' -> '$2'"
        ln -sf $1 $2
    else
        echo -e "\e[91mWARNING: $2 exists and is not a link!\e[0m"
    fi
}

# inputrc and bash
linkDotfile ~/.dotfiles/bash/inputrc ~/.inputrc
linkDotfile ~/.dotfiles/bash/bash_profile ~/.bash_profile
linkDotfile ~/.dotfiles/bash/bashrc ~/.bashrc

# tmux
linkDotfile ~/.dotfiles/tmux.conf ~/.tmux.conf

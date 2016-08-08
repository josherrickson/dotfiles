#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

lns() {
if [[ !(-a "$2") && (-h "$2") ]]; then
    echo invalid symlink "$2", replacing
    rm "$2"
    ln -s "$1" "$2"
elif [[ (-a "$2") && ( -h "$2") ]]; then
    echo valid symlink "$2" exists, doing nothing
elif [[ (-a "$2") && !( -h "$2") ]]; then
    echo "$2" is a real file, backing up and symlinking
    mv "$2" "$2"_$(date '+%m.%d.%y_%H.%M.%S')
    ln -s "$1" "$2"
elif [[ !(-a "$2") && !( -h "$2") ]]; then
    echo no file, creating symlink
    echo ln -s "$1" "$2"
    ln -s "$1" "$2"
fi
}

lns "$DIR"/.aliases ~/.aliases
lns "$DIR"/.Rprofile ~/.Rprofile
lns "$DIR"/.zshrc ~/.zshrc
lns "$DIR"/init.el ~/.emacs.d/init.el

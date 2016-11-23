#!/bin/bash
NORMAL=$(tput sgr0)
GREEN=$(tput setaf 2; tput bold)
YELLOW=$(tput setaf 3)
RED=$(tput setaf 1)

function red() {
    echo -e "$RED$*$NORMAL"
}

function green() {
    echo -e "$GREEN$*$NORMAL"
}

function yellow() {
    echo -e "$YELLOW$*$NORMAL"
}

ostype=$(uname)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

lns() {
if [[ !(-a "$2") && (-h "$2") ]]; then
    red invalid symlink "$2", replacing
    rm "$2"
    ln -s "$1" "$2"
elif [[ (-a "$2") && ( -h "$2") ]]; then
    yellow valid symlink "$2" exists, doing nothing
elif [[ (-a "$2") && !( -h "$2") ]]; then
    red "$2" is a real file, backing up and symlinking
    mv "$2" "$2"_$(date '+%m.%d.%y_%H.%M.%S')
    ln -s "$1" "$2"
elif [[ !(-a "$2") && !( -h "$2") ]]; then
    green no file, creating symlink
    echo ln -s "$1" "$2"
    ln -s "$1" "$2"
fi
}

lns "$DIR"/.aliases ~/.aliases
lns "$DIR"/.Rprofile ~/.Rprofile
lns "$DIR"/.zshrc ~/.zshrc
lns "$DIR"/.tmux.conf ~/.tmux.conf
lns "$DIR"/.vimrc ~/.vimrc
lns "$DIR"/.gtkrc-2.0 ~/.gtkrc-2.0
[[ ! (-d ~/.emacs.d) ]] && mkdir ~/.emacs.d
lns "$DIR"/init.el ~/.emacs.d/init.el


if [[ $ostype == 'Linux' ]]; then
    lns "$DIR"/.Xresources ~/.Xresources
    xrdb ~/.Xresources

    [[ ! (-d ~/.xmonad) ]] && mkdir ~/.xmonad
    lns "$DIR"/xmobar.rc ~/.xmonad/xmobar.rc
    lns "$DIR"/xmonad.hs ~/.xmonad/xmonad.hs
    lns "$DIR"/xmonad-init ~/.xmonad/xmonad-init

    [[ ! (-d ~/.i3) ]] && mkdir ~/.i3
    lns "$DIR"/i3config ~/.i3/config
    lns "$DIR"/i3status.conf ~/.i3/i3status.conf

fi

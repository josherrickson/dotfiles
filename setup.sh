#!/bin/bash
ostype=$(uname)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -s "$DIR"/.aliases ~/.
ln -s "$DIR"/.Rprofile ~/.
ln -s "$DIR"/.zshrc ~/.
ln -s "$DIR"/.vimrc ~/.
ln -s "$DIR"/.gtkrc-2.0 ~/.
ln -s "$DIR"/init.el ~/.emacs.d/.

if [[ $ostype == 'Linux' ]]; then
    ln -s "$DIR"/.Xresources ~/.
    xrdb ~/.Xresources

    ln -s "$DIR"/xmobar.rc ~/.xmonad/.
    ln -s "$DIR"/xmonad.hs ~/.xmonad/.
    ln -s "$DIR"/xmonad-init ~/.xmonad/.

    ln -s "$DIR"/luakit/*.lua ~/.config/luakit/.
    ln -s "$DIR"/luakit/adblock ~/.local/share/luakit/.
fi

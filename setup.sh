#!/bin/bash


ln -s ~/dotfiles/.aliases ~/.
ln -s ~/dotfiles/.Rprofile ~/.
ln -s ~/dotfiles/.zshrc ~/.
ln -s ~/dotfiles/.gtkrc-2.0 ~/.
ln -s ~/dotfiles/.Xresources ~/.
xrdb ~/.Xresources

ln -s ~/dotfiles/init.el ~/.emacs.d/

ln -s ~/dotfiles/xmobar.rc ~/.xmonad/
ln -s ~/dotfiles/xmonad.hs ~/.xmonad/
ln -s ~/dotfiles/xmonad-init ~/.xmonad/

ln -s ~/dotfiles/luakit/*.lua ~/.config/luakit/
ln -s ~/dotfiles/luakit/adblock ~/.local/share/luakit/

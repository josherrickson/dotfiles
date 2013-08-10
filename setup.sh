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

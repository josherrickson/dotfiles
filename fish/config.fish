# No greeting
set fish_greeting

# PWD shouldn't shrink directory names
set -g fish_prompt_pwd_dir_length 0

set -xg EDITOR emacs

# For some reason, when using fish inside tmux in iTerm2, $TERM is "screen". This causes
# emacs to think it doesn't have colors to properly display themes. (For some reason,
# you need all three of those. fish in tmux in Terminal.app has proper $TERM, zsh in tmux
# in iTerm2 has proper $TERM.)
set -xg TERM xterm-256color

set -xg PATH /opt/homebrew/bin $PATH /Users/josh/.local/bin ~/.cargo/bin

# Color for man
set -xU LESS_TERMCAP_md (printf "\e[01;31m")
set -xU LESS_TERMCAP_me (printf "\e[0m")
set -xU LESS_TERMCAP_se (printf "\e[0m")
set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")
set -xU LESS_TERMCAP_ue (printf "\e[0m")
set -xU LESS_TERMCAP_us (printf "\e[01;32m")In
set -xU LESS "--RAW-CONTROL-CHARS"

# Color for less. Requires highlight installed.
export LESSOPEN="| highlight %s --out-format xterm256 --force"

# Syncs dircolors with LS_COLOR for treex
eval (gdircolors -c)

# I prefer alt-p for previous command
bind \ep up-or-search
bind \eP __fish_paginate

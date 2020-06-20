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

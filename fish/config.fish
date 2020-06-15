# No greeting
set fish_greeting

# PWD shouldn't shrink directory names
set -g fish_prompt_pwd_dir_length 0



alias grep='grep --color=always'
alias fgrep='fgrep --color=always'
alias tmux='tmux -2'
alias ls='ls -AhFG'
alias lst='date;ls -lt | head'

alias R='R --quiet'

alias emacs='emacs -nw'

alias install_R_deps='bash ~/.bash-scripts/install_R_deps.sh'

alias rmds_store='find ~ -name ".DS_Store" -depth -exec rm {} \;'

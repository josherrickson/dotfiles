###################
##### History #####
###################

HISTFILE=~/.zshhist       # where to save history
HISTSIZE=1000000          # how many lines of history to keep
                          # internally
SAVEHIST=1000000          # how many lines of history to write to
                          # HISTFILE
setopt INC_APPEND_HISTORY # append rather than overwrite the history
                          # file
setopt SHARE_HISTORY      # allow multiple instances to write from
                          # history
setopt HIST_IGNORE_DUPS   # Sequential duplicate commands only get one
                          # history entry.

export LESSHISTFILE=/dev/null # less doesn't need to pollute with a
                              # history

#######################
##### job counter #####
#######################

# Count the number of jobs (if 0, return nothing, not 0) to use in
# PROMPT later
precmd(){
    psvar[2]=$#jobstates; [[ $psvar[2] -eq 0 ]] && psvar[2]=() }

#########################
##### auto-complete #####
#########################

if [[ $(uname) == 'Darwin' ]]; then
    zstyle :compinstall filename '/Users/josh/.zshrc'
elif [[ $(uname)  == 'Linux' ]]; then
    zstyle :compinstall filename '/home/josh/.zshrc'
fi
zstyle ':completion:*' menu select # sets completion to be menu driven
zstyle ':completion:*' hosts off   # Don't uses my hosts file (since I
                                   # use it for domain blacklisting)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # ignore case in
                                                    # tab complete

autoload -Uz compinit
compinit

unsetopt CORRECT CORRECT_ALL       # don't correct spelling

################
##### Path #####
################

if [[ $(uname) == 'Darwin' ]]; then
    PATH=/usr/local/bin:$PATH:/Library/TeX/texbin:/usr/local/sbin
elif [[ $(uname)  == 'Linux' ]]; then
    PATH=/usr/local/texlive/2013/bin/x86_64-linux:$PATH:/usr/local/share/python
    INFOPATH=/usr/local/texlive/2013/texmf-dist/doc/info:$INFOPATH
    MANPATH=/usr/local/texlive/2013/texmf-dist/doc/man:$MANPATH
fi
export PATH

PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export PYTHONPATH

##################
##### Prompt #####
##################

# Enable colors in prompts
autoload -U colors && colors

#### Get Git info ####
# http://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh
setopt prompt_subst
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git cvs svn
# During special actions like rebasing
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
# During normal use
zstyle ':vcs_info:*' formats '%F{4}[%b]%f '
# For branching on SVN
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{3}:%r%F{4}'

# or use pre_cmd, see man zshcontrib
vcs_info_wrapper() {
    vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
    fi
}

## PROMPT is where the cursor is at, RPROMPT is on the right-hand-side
# left prompt is >>> by default, but (#)>>> when there are # jobs in
# the background
# right prompt is current dir
PROMPT=$'%{$fg[red]%}%(2v:(%2v):)%{$reset_color%}%{$fg[green]%}>>> %{$reset_color%}'
RPROMPT=$'$(vcs_info_wrapper)%{$fg[green]%}%~%{$reset_color%}%'

#############################
##### edit-command-line #####
#############################

# Calling C-x C-e while typing a command will open the command in
# EDITOR. Easy to edit a complicated command.
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

########################
##### Colorize man #####
########################

export TERM=xterm-256color # needed to get correct coloring

export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info
                                          # box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

#####################
##### Functions #####
#####################

# From https://news.ycombinator.com/item?id=6310109
function bd () {
  OLDPWD=`pwd`
  NEWPWD=`echo $OLDPWD | sed 's|\(.*/'$1'[^/]*/\).*|\1|'`
  index=`echo $NEWPWD | awk '{ print index($1,"/'$1'"); }'`
  if [ $index -eq 0 ] ; then
    echo "No such occurrence."
  else
    cd "$NEWPWD"
  fi
}

# From https://github.com/dbb/githome
function font_test () {
    print "Letters:\tAaBbCcDdEeFfGgHhIiJjKkLlMm"
    print "\t\tNnOoPpQqRrSsTtUuVvWwXxYyZz"
    print "Digits:\t\t0123456789"
    print "Brackets:\t( ) [ ] { } < > "
    print "Quotes:\t\t\"foo\" 'bar' "
    print "Punctuation: \t, . ? : ; _ ! "
    print "Symbols:\t\` ~  @ # $ % ^ & * - + = | / \\"
    print "Ambiguity:\t1Il ij ao DO0Q B8 \`\`''\" ({ ,. ;: "
    ls -AhFG
}



################
##### Misc #####
################

export EDITOR="emacs -nw"      # for short edits, the terminal is
                               # faster
setopt EXTENDED_GLOB           # more options for matching
setopt nobeep                  # no beeping!
setopt longlistjobs            # gives more information when
                               # suspending a job
setopt AUTO_RESUME             # Commands without arguments will first
                               # try to resume suspended programs of
                               # the same name.
bindkey -e                     # Emacs controls
#bindkey -v                    # Vi controls
bindkey -M vicmd "q" push-line # Use alt-q to store a line, let you
                               # run a new line, then restore the
                               # first line

autoload -U select-word-style
select-word-style bash         # For ctrl-w to NOT annihilate an
                               # entire path

autoload -Uz zmv               # For mass renaming eg 'noglob zmv -W
                               # *.txt.old org/*.txt'

#######################
##### Other Files #####
#######################

source ~/.aliases
source ~/.aliases-private
# clone from git@github.com:josherrickson/bashmarks.git
source ~/repositories/bashmarks/bashmarks.sh

###################
##### History #####
###################

HISTFILE=~/.zshhist           # where to save history
HISTSIZE=1000000              # how many lines of history to keep
                              # internally
SAVEHIST=1000000              # how many lines of history to write to
                              # HISTFILE
setopt INC_APPEND_HISTORY     # append rather than overwrite the history
                              # file
setopt SHARE_HISTORY          # allow multiple instances to write from
                              # history
setopt HIST_IGNORE_ALL_DUPS   # Duplicate commands only get one
                              # history entry.

export LESSHISTFILE=/dev/null # less doesn't need to pollute with a
                              # history

############################
##### Color definition #####
############################

LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:'
export LS_COLORS

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
else
    zstyle :compinstall filename '/home/josh/.zshrc'
fi
zstyle ':completion:*' menu select # sets completion to be menu driven
zstyle ':completion:*' hosts off   # Don't uses my hosts file (since I
                                   # use it for domain blacklisting)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # ignore case in
                                                    # tab complete
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -Uz compinit
compinit

unsetopt CORRECT CORRECT_ALL       # don't correct spelling

################
##### Path #####
################

PATH=/opt/homebrew/bin:/usr/local/bin:$PATH:~/anaconda/bin:~/.cargo/bin:~/Library/Python/3.7/bin:/Library/TeX/texbin:/usr/local/sbin:/Applications/Stata/StataSE.app/Contents/MacOS:~/.local/bin
export PATH

##################
##### Prompt #####
##################

# Enable colors in prompts
autoload -U colors && colors

#### Get Git info ####
# http://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh
setopt prompt_subst
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git
# During special actions like rebasing
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
# During normal use:
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '!'
zstyle ':vcs_info:*' stagedstr '!'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked

# Untracked files, send to Misc (%m)
# from https://stackoverflow.com/a/49744699
+vi-git-untracked() {
  if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
     git status --porcelain | grep -m 1 '^??' &>/dev/null
  then
    hook_com[misc]='?'
  fi
}

zstyle ':vcs_info:*' formats '%F{4}[%b%F{red}%u%m%f%F{yellow}%c%f]%f '

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
if [[ $(uname) == 'Darwin' ]]; then
    PROMPT=$'%{$fg[red]%}%(2v:(%2v):)%{$reset_color%}%{$fg[green]%}>>> %{$reset_color%}'
elif [[ $(uname)  == 'Linux' ]]; then
    PROMPT=$'%{$fg[red]%}%(2v:(%2v):)%{$reset_color%}%{$fg[blue]%}[%m]>>> %{$reset_color%}'
fi
RPROMPT=$'$(vcs_info_wrapper)%{$fg[green]%}%~ [%T]%{$reset_color%}%'
# Only difference between versions is color to easier identify when on the server


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

export EDITOR="emacs"
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

####################
##### Homebrew #####
####################

export HOMEBREW_NO_INSTALL_CLEANUP=0

#######################
##### Other Files #####
#######################

[ -f ~/.aliases ] && source ~/.aliases
[ -f ~/.aliases-private ] && source ~/.aliases-private

# Homewbrew stores things in different locations on Intel/Arm

# zsh-syntax-hightlighting
# https://github.com/zsh-users/zsh-syntax-highlighting/
# Install via homebrew
if [[ $(uname -m) == 'arm64' ]]; then
  zshsyntax=/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [[ $(uname -m) == 'x86_64' ]]; then
  zshsyntax=/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
[ -f $zshsyntax ] && source $zshsyntax

# zsh-autosuggestions
# https://github.com/zsh-users/zsh-autosuggestions
# Install via homebrew
if [[ $(uname -m) == 'arm64' ]]; then
    zshautosuggest=/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
elif [[ $(uname -m) == 'x86_64' ]]; then
    zshautosuggest=/usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
[ -f $zshautosuggest ] && source $zshautosuggest

# Enable fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

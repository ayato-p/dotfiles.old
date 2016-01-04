# Key binding - Emacs
bindkey -e
# Disable to default C-q
stty start undef

# for Hisotry
HISTSIZE=9999
SAVEHIST=9999
HISTFILE=~/.zsh_history
setopt histignorealldups hist_reduce_blanks hist_no_store hist_verify share_history

# for Prompt
autoload -Uz vcs_info
autoload -U promptinit
setopt prompt_subst
promptinit

prompt pure

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!%f"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+%f"
zstyle ':vcs_info:*' formats "%c%u%F{cyan}**%f%B%F{white}>%f%F{black}(%f%F{green}%b%f%F{black})%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
    vcs_info
}
# RPROMPT='${vcs_info_msg_0_}'

# for Completion
autoload -Uz compinit
compinit

setopt complete_in_word

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'


# User definition

alias emacsd="emacs &"
alias ekill='emacsclient -e "(kill-emacs)"'
alias e="emacsclient -nw"

export EDITOR='emacsclient -nw'
export TERM=screen-256color

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/.ndenv/bin:$PATH
export PATH=$PATH:/usr/local/go/bin

# Stop warning with Emacs
export NO_AT_BRIDGE=1

eval "$(ndenv init -)"

function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(history -n 1 | \
                    eval $tac | \
                    peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

typeset -U chpwd_functions
CD_HISTORY_FILE=${HOME}/.cd_history_file
function chpwd_record_history() {
    echo $PWD >> ${CD_HISTORY_FILE}
}
chpwd_functions=($chpwd_functions chpwd_record_history)

function peco_get_destination_from_history() {
    sort ${CD_HISTORY_FILE} | uniq -c | sort -r | \
        sed -e 's/^[ ]*[0-9]*[ ]*//' | \
        sed -e s"/^${HOME//\//\\/}/~/" | \
        peco | xargs echo
}

function peco_cd_history() {
    local destination=$(peco_get_destination_from_history)
    [ -n $destination ] && cd ${destination/#\~/${HOME}}
    zle reset-prompt
}
zle -N peco_cd_history

function peco_insert_history() {
    local destination=$(peco_get_destination_from_history)
    if [ $? -eq 0 ]; then
        local new_left="${LBUFFER} ${destination} "
        BUFFER=${new_left}${RBUFFER}
        CURSOR=${#new_left}
    fi
    zle reset-prompt
}
zle -N peco_insert_history

bindkey '^x;' peco_cd_history
bindkey '^xi' peco_insert_history
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export SASS_LIBSASS_PATH="$HOME/lib/libsass"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# start vmware-user
start-vmware-user

# tmux
if [ -z $TMUX ] ; then
    if [ -z `tmux ls` ] ; then
        tmux
    else
        tmux attach
    fi
fi

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

PURE_PROMPT_SYMBOL="λ"

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

export PATH=$HOME/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH

# Language tools
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/go/bin

export PATH=$HOME/.ndenv/bin:$PATH
eval "$(ndenv init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export SASS_LIBSASS_PATH="$HOME/lib/libsass"

# Stop warning with Emacs
export NO_AT_BRIDGE=1

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# For fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS="--reverse --inline-info --bind=ctrl-k:kill-line"


# zplug
source ~/.zplug/zplug

zplug "b4b4r07/enhancd", of:enhancd.sh
export ENHANCD_FILTER=fzf-tmux:fzf

zplug load

# Start vmware-user
start-vmware-user

# tmux
if [ -z $TMUX ] ; then
    if [ -z `tmux ls 2> /dev/null` ] ; then
        tmux
    else
        tmux attach
    fi
else
    kuromadoushi
fi
